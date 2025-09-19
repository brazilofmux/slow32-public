//===- SLOW32LoadAddrOpt.cpp - Fold LOAD_ADDR addends --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// A simple MachineFunction pass that canonicalises address materialisation by
// folding `ADDI` immediates into the preceding `LOAD_ADDR` pseudo. This keeps
// global+constant forms flowing through to the post-RA expansion so we always
// emit a single LUI/ADDI pair with the final relocation.
//
//===----------------------------------------------------------------------===//

#include "SLOW32.h"
#include "SLOW32InstrInfo.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-load-addr-opt"

namespace {

class SLOW32LoadAddrOpt : public MachineFunctionPass {
public:
  static char ID;
  SLOW32LoadAddrOpt() : MachineFunctionPass(ID) {
    initializeSLOW32LoadAddrOptPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override {
    return "SLOW32 load address folding";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  bool foldAddIntoLoadAddr(MachineInstr &Add, MachineRegisterInfo &MRI,
                           const TargetInstrInfo *TII) const;
};

} // namespace

char SLOW32LoadAddrOpt::ID = 0;

INITIALIZE_PASS(SLOW32LoadAddrOpt, DEBUG_TYPE, "SLOW32 load address folding",
                false, false)

static bool canAdjustOperand(const MachineOperand &Op) {
  return Op.isGlobal() || Op.isSymbol() || Op.isMCSymbol() ||
         Op.isBlockAddress() || Op.isCPI();
}

static bool adjustOperandOffset(MachineOperand &Op, int64_t Delta) {
  if (Delta == 0)
    return true;

  if (Op.isGlobal() || Op.isSymbol() || Op.isMCSymbol() || Op.isBlockAddress() ||
      Op.isCPI()) {
    Op.setOffset(Op.getOffset() + Delta);
    return true;
  }

  return false;
}

bool SLOW32LoadAddrOpt::foldAddIntoLoadAddr(MachineInstr &Add,
                                            MachineRegisterInfo &MRI,
                                            const TargetInstrInfo *TII) const {
  if (Add.getOpcode() != SLOW32::ADDI)
    return false;

  if (!Add.getOperand(1).isReg() || !Add.getOperand(2).isImm())
    return false;

  Register BaseReg = Add.getOperand(1).getReg();
  MachineInstr *Def = nullptr;

  if (BaseReg.isVirtual())
    Def = MRI.getVRegDef(BaseReg);
  else if (Register::isPhysicalRegister(BaseReg))
    return false;

  if (!Def || Def->getOpcode() != SLOW32::LOAD_ADDR)
    return false;

  MachineOperand &SymOp = Def->getOperand(1);
  if (!canAdjustOperand(SymOp))
    return false;

  int64_t Imm = Add.getOperand(2).getImm();

  Register DstReg = Add.getOperand(0).getReg();

  // Case 1: the ADDI writes back into the same register and the original
  // LOAD_ADDR has no other users. In that case we can mutate the LOAD_ADDR in
  // place and erase the ADDI entirely.
  if (DstReg == BaseReg && MRI.hasOneNonDBGUse(BaseReg)) {
    if (adjustOperandOffset(SymOp, Imm)) {
      LLVM_DEBUG(dbgs() << "SLOW32LoadAddrOpt: folded ADDI into defining "
                        "LOAD_ADDR (in-place)\n");
      Add.eraseFromParent();
      return true;
    }
    return false;
  }

  // Otherwise synthesise a fresh LOAD_ADDR carrying the combined offset so the
  // ADDI can be removed without perturbing other uses of the original result.
  MachineOperand NewOp = SymOp;
  if (!adjustOperandOffset(NewOp, Imm))
    return false;

  MachineBasicBlock &MBB = *Add.getParent();
  MachineInstrBuilder MIB =
      BuildMI(MBB, Add, Add.getDebugLoc(), TII->get(SLOW32::LOAD_ADDR), DstReg);
  MIB.add(NewOp);

  LLVM_DEBUG(dbgs() << "SLOW32LoadAddrOpt: materialised LOAD_ADDR with folded "
                    "offset for ADDI\n");

  Add.eraseFromParent();
  return true;
}

bool SLOW32LoadAddrOpt::runOnMachineFunction(MachineFunction &MF) {
  const SLOW32Subtarget &STI = MF.getSubtarget<SLOW32Subtarget>();
  const TargetInstrInfo *TII = STI.getInstrInfo();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  bool Changed = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineBasicBlock::iterator MII = MBB.begin(), E = MBB.end();
         MII != E;) {
      MachineInstr &MI = *MII++;
      Changed |= foldAddIntoLoadAddr(MI, MRI, TII);
    }
  }

  return Changed;
}

FunctionPass *llvm::createSLOW32LoadAddrOptPass() {
  return new SLOW32LoadAddrOpt();
}
