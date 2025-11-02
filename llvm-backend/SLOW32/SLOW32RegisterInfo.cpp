//===-- SLOW32RegisterInfo.cpp - SLOW32 Register Information ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SLOW32 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "SLOW32RegisterInfo.h"
#include "SLOW32.h"
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

SLOW32RegisterInfo::SLOW32RegisterInfo() : SLOW32GenRegisterInfo(SLOW32::R31) {}

const MCPhysReg *
SLOW32RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  static const MCPhysReg CalleeSavedRegs[] = {
    SLOW32::R11, SLOW32::R12, SLOW32::R13, SLOW32::R14,
    SLOW32::R15, SLOW32::R16, SLOW32::R17, SLOW32::R18,
    SLOW32::R19, SLOW32::R20, SLOW32::R21, SLOW32::R22,
    SLOW32::R23, SLOW32::R24, SLOW32::R25, SLOW32::R26,
    SLOW32::R27, SLOW32::R28,
    SLOW32::R30, SLOW32::R31,
    0
  };
  return CalleeSavedRegs;
}

BitVector SLOW32RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(SLOW32::R0);   // r0 is always zero
  Reserved.set(SLOW32::R2);   // Reserved for long-branch materialisation
  Reserved.set(SLOW32::R29);  // Stack pointer (sp)
  Reserved.set(SLOW32::R30);  // Frame pointer (fp)
  return Reserved;
}

bool SLOW32RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                              int SPAdj, unsigned FIOperandNum,
                                              RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  
  // Get the frame index operand
  MachineOperand &FIOp = MI.getOperand(FIOperandNum);
  int FrameIndex = FIOp.getIndex();
  
  // Calculate the actual offset from the frame pointer
  int64_t Offset = MFI.getObjectOffset(FrameIndex);
  
  // Stack pointer adjustments between call frames are already baked into the
  // prologue/epilogue. Just account for them here so we keep consistent offsets.
  Offset += SPAdj;

  // Frame indexes currently enter without an additional offset. If we start
  // using addFrameIndex with non-zero displacements we will thread that here.

  // Ensure the base+offset fits in the 12-bit signed window supported by the
  // instruction. If not, materialise the high part using additional ADDI nodes
  // so the final memory op keeps a legal displacement.
  MachineOperand &BaseOp = MI.getOperand(FIOperandNum - 1);
  Register BaseReg = BaseOp.getReg();
  DebugLoc DL = MI.getDebugLoc();

  if (!isInt<12>(Offset)) {
    if (RS) {
      if (!BaseReg.isPhysical())
        report_fatal_error("Frame index elimination requires physical base regs");

      unsigned Scav = RS->scavengeRegisterBackwards(SLOW32::GPRRegClass, II,
                                                    false, SPAdj);
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

      Offset = Remaining;
      BaseOp.setReg(Scratch);
      BaseOp.setIsKill(false);
      RS->setRegUsed(Scav);
    } else {
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

      Offset = Remaining;
      BaseOp.setReg(CurrBase);
      BaseOp.setIsKill(false);
    }
  }

  FIOp.ChangeToImmediate(Offset);
  
  return false;
}

Register SLOW32RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return SLOW32::R30;  // FP register
}

bool SLOW32RegisterInfo::requiresRegisterScavenging(const MachineFunction &MF) const {
  return true;
}
