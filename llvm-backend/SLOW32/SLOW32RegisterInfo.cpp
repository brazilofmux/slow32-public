//===-- SLOW32RegisterInfo.cpp - SLOW32 Register Information ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32RegisterInfo.h"
#include "SLOW32.h"
#include "SLOW32FrameLowering.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>

#define GET_REGINFO_TARGET_DESC
#include "SLOW32GenRegisterInfo.inc"

using namespace llvm;

SLOW32RegisterInfo::SLOW32RegisterInfo()
    : SLOW32GenRegisterInfo(SLOW32::R31, /*DwarfFlavour=*/0, /*EHFlavor=*/0,
                            /*PC=*/0) {}

const MCPhysReg *
SLOW32RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SLOW32_SaveList;
}

const uint32_t *
SLOW32RegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                         CallingConv::ID) const {
  return CSR_SLOW32_RegMask;
}

BitVector SLOW32RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  // markSuperRegs also reserves containing GPRPair super-registers.
  markSuperRegs(Reserved, SLOW32::R0); // r0 is always zero
  // r2: machine long-branch materialisation AND MC branch-relaxation
  // scratch (AsmBackend hard-wires r2). Handwritten asm that relaxes
  // must treat r2 as clobbered.
  markSuperRegs(Reserved, SLOW32::R2);
  markSuperRegs(Reserved, SLOW32::R29); // Stack pointer (sp)
  // FP is only reserved when the function actually uses a frame pointer.
  // Leaving it free when !hasFP lets the allocator use r30 as a normal temp.
  if (getFrameLowering(MF)->hasFP(MF))
    markSuperRegs(Reserved, SLOW32::R30);
  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

/// Materialise BaseReg + Offset into a register that can be used as a memory
/// base, returning the residual simm12 displacement that still needs to sit
/// in the memory instruction's imm field.
static int64_t materialiseBasePlusOffset(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator II, const DebugLoc &DL,
    const TargetInstrInfo *TII, MachineRegisterInfo &MRI, Register BaseReg,
    int64_t Offset, int SPAdj, RegScavenger *RS, Register &OutBase) {
  if (isInt<12>(Offset)) {
    OutBase = BaseReg;
    return Offset;
  }

  if (RS) {
    if (!BaseReg.isPhysical())
      report_fatal_error("Frame index elimination requires physical base regs");

    unsigned Scav =
        RS->scavengeRegisterBackwards(SLOW32::GPRRegClass, II, false, SPAdj);
    if (!Scav)
      report_fatal_error("Unable to scavenge register for frame index");

    Register Scratch = Register(Scav);
    BuildMI(MBB, II, DL, TII->get(SLOW32::ADD), Scratch)
        .addReg(BaseReg)
        .addReg(SLOW32::R0);

    int64_t Remaining = Offset;
    while (!isInt<12>(Remaining)) {
      int64_t Step = Remaining > 0 ? std::min<int64_t>(Remaining, 2047)
                                   : std::max<int64_t>(Remaining, -2048);
      BuildMI(MBB, II, DL, TII->get(SLOW32::ADDI), Scratch)
          .addReg(Scratch)
          .addImm(Step);
      Remaining -= Step;
    }
    OutBase = Scratch;
    RS->setRegUsed(Scav);
    return Remaining;
  }

  Register CurrBase = BaseReg;
  int64_t Remaining = Offset;
  while (!isInt<12>(Remaining)) {
    int64_t Step = Remaining > 0 ? std::min<int64_t>(Remaining, 2047)
                                 : std::max<int64_t>(Remaining, -2048);
    Register NextBase = MRI.createVirtualRegister(&SLOW32::GPRRegClass);
    BuildMI(MBB, II, DL, TII->get(SLOW32::ADDI), NextBase)
        .addReg(CurrBase)
        .addImm(Step);
    CurrBase = NextBase;
    Remaining -= Step;
  }
  OutBase = CurrBase;
  return Remaining;
}

bool SLOW32RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                             int SPAdj, unsigned FIOperandNum,
                                             RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  DebugLoc DL = MI.getDebugLoc();

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();

  // Canonical frame reference: base register + fixed offset from the
  // frame-pointer (or SP when !hasFP). SLOW32FrameLowering overrides the
  // default so the manual LR/FP save slots at the bottom of the frame are
  // accounted for.
  Register FrameReg;
  int64_t Offset =
      TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();
  Offset += SPAdj;

  unsigned Opc = MI.getOpcode();

  // GPRPair spill pseudos: (base, val/def, FI, half-off) → real LDW/STW.
  if (Opc == SLOW32::STW_FI || Opc == SLOW32::LDW_FI) {
    assert(FIOperandNum + 1 < MI.getNumOperands() &&
           MI.getOperand(FIOperandNum + 1).isImm() &&
           "STW_FI/LDW_FI must carry a half-offset immediate");
    Offset += MI.getOperand(FIOperandNum + 1).getImm();
    MI.removeOperand(FIOperandNum + 1);
    Opc = (Opc == SLOW32::STW_FI) ? SLOW32::STW : SLOW32::LDW;
    MI.setDesc(TII->get(Opc));
  }

  // Memory layout after (optional) STW_FI rewrite:
  //   ST*: op0=base, op1=val, op2=imm
  //   LD*: op0=def,  op1=base, op2=imm
  // FI may live in either the base slot (SLOW32Addr) or the imm slot
  // (legacy storeRegToStackSlot: STW FrameReg, val, FI).
  const bool IsStore =
      Opc == SLOW32::STW || Opc == SLOW32::STH || Opc == SLOW32::STB;
  const bool IsLoad = Opc == SLOW32::LDW || Opc == SLOW32::LDB ||
                      Opc == SLOW32::LDBU || Opc == SLOW32::LDH ||
                      Opc == SLOW32::LDHU;

  if (IsStore || IsLoad) {
    const unsigned BaseIdx = IsStore ? 0u : 1u;
    const unsigned ImmIdx = IsStore ? 2u : 2u;
    const bool FIIsBase = (FIOperandNum == BaseIdx);

    if (FIIsBase) {
      // SelectAddr form: base=FI, imm=extra displacement.
      assert(MI.getOperand(ImmIdx).isImm());
      Offset += MI.getOperand(ImmIdx).getImm();

      Register OutBase;
      int64_t Imm =
          materialiseBasePlusOffset(MBB, II, DL, TII, MRI, FrameReg, Offset,
                                    SPAdj, RS, OutBase);
      MI.getOperand(BaseIdx).ChangeToRegister(OutBase, /*IsDef=*/false);
      MI.getOperand(ImmIdx).ChangeToImmediate(Imm);
      return false;
    }

    // Legacy form: base is already a register (usually FrameReg), FI is imm.
    assert(FIOperandNum == ImmIdx && "Unexpected FI operand position");
    // Prefer the canonical FrameReg from getFrameIndexReference; if the
    // instruction already carries a different base (shouldn't happen for
    // stack spills), keep it and only rewrite the offset.
    Register BaseReg = MI.getOperand(BaseIdx).getReg();
    if (BaseReg == SLOW32::R30 || BaseReg == SLOW32::R29 || !BaseReg)
      BaseReg = FrameReg;

    Register OutBase;
    int64_t Imm = materialiseBasePlusOffset(MBB, II, DL, TII, MRI, BaseReg,
                                            Offset, SPAdj, RS, OutBase);
    MI.getOperand(BaseIdx).ChangeToRegister(OutBase, /*IsDef=*/false);
    MI.getOperand(BaseIdx).setIsKill(false);
    MI.getOperand(ImmIdx).ChangeToImmediate(Imm);
    return false;
  }

  // Non-memory uses of FI.
  //
  // Shape A — FI stands in for an immediate (legacy FrameIndex select and
  //           stack spill form): `ADDI rd, FrameReg, FI` or `STW base, val, FI`.
  //           Detected when the desc marks this slot as Imm, or when the
  //           previous operand is a register and there is no following imm.
  // Shape B — FI is a base register with a following imm: `ADDI rd, FI, imm`.
  const MCOperandInfo &OpInfo = MI.getDesc().operands()[FIOperandNum];
  const bool DescSaysImm = OpInfo.OperandType == MCOI::OPERAND_IMMEDIATE;
  const bool PrevIsReg =
      FIOperandNum > 0 && MI.getOperand(FIOperandNum - 1).isReg();
  const bool NextIsImm = FIOperandNum + 1 < MI.getNumOperands() &&
                         MI.getOperand(FIOperandNum + 1).isImm();
  const bool FIInImmSlot = DescSaysImm || (PrevIsReg && !NextIsImm);

  if (FIInImmSlot) {
    Register OutBase;
    int64_t Imm = materialiseBasePlusOffset(MBB, II, DL, TII, MRI, FrameReg,
                                            Offset, SPAdj, RS, OutBase);
    if (PrevIsReg) {
      MI.getOperand(FIOperandNum - 1)
          .ChangeToRegister(OutBase, /*IsDef=*/false);
      MI.getOperand(FIOperandNum - 1).setIsKill(false);
    }
    MI.getOperand(FIOperandNum).ChangeToImmediate(Imm);
    return false;
  }

  if (NextIsImm) {
    Offset += MI.getOperand(FIOperandNum + 1).getImm();
    Register OutBase;
    int64_t Imm = materialiseBasePlusOffset(MBB, II, DL, TII, MRI, FrameReg,
                                            Offset, SPAdj, RS, OutBase);
    MI.getOperand(FIOperandNum).ChangeToRegister(OutBase, /*IsDef=*/false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Imm);
    return false;
  }

  // Bare FI as a pure register operand: materialise the absolute address.
  Register OutBase;
  int64_t Imm = materialiseBasePlusOffset(MBB, II, DL, TII, MRI, FrameReg,
                                          Offset, SPAdj, RS, OutBase);
  if (Imm != 0) {
    if (RS) {
      unsigned Scav = RS->scavengeRegisterBackwards(SLOW32::GPRRegClass, II,
                                                    false, SPAdj);
      if (!Scav)
        report_fatal_error("Unable to scavenge for frame address");
      Register Scratch = Register(Scav);
      BuildMI(MBB, II, DL, TII->get(SLOW32::ADDI), Scratch)
          .addReg(OutBase)
          .addImm(Imm);
      OutBase = Scratch;
      RS->setRegUsed(Scav);
    } else {
      Register Addr = MRI.createVirtualRegister(&SLOW32::GPRRegClass);
      BuildMI(MBB, II, DL, TII->get(SLOW32::ADDI), Addr)
          .addReg(OutBase)
          .addImm(Imm);
      OutBase = Addr;
    }
  }
  MI.getOperand(FIOperandNum).ChangeToRegister(OutBase, /*IsDef=*/false);
  return false;
}

Register
SLOW32RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  return TFI->hasFP(MF) ? SLOW32::R30 : SLOW32::R29;
}

bool SLOW32RegisterInfo::requiresRegisterScavenging(
    const MachineFunction &MF) const {
  return true;
}
