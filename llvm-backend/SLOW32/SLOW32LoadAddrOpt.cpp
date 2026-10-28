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
#include <optional>

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
  bool foldLoIntoMemOps(MachineInstr &LA, MachineRegisterInfo &MRI,
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

// The base operand of a load or store, which is followed by the immediate.
static bool memBaseIndex(unsigned Opcode, unsigned &BaseIdx) {
  switch (Opcode) {
  case SLOW32::LDB:
  case SLOW32::LDBU:
  case SLOW32::LDH:
  case SLOW32::LDHU:
  case SLOW32::LDW:
    BaseIdx = 1;
    return true;
  case SLOW32::STB:
  case SLOW32::STH:
  case SLOW32::STW:
    BaseIdx = 0;
    return true;
  default:
    return false;
  }
}

// An address used only as the base of loads and stores, all at the same
// offset: "lui %hi(sym+off)" and %lo(sym+off) in each of them, in place of
// lui/addi and an offset -- one instruction fewer per address.  A word of a
// static struct (which GlobalOpt often splits into one global per field)
// cost lui+addi+ldw.  Different offsets keep the shared base: %hi of two
// addends can differ by the carry out of %lo, which the link decides.
bool SLOW32LoadAddrOpt::foldLoIntoMemOps(MachineInstr &LA,
                                         MachineRegisterInfo &MRI,
                                         const TargetInstrInfo *TII) const {
  if (LA.getOpcode() != SLOW32::LOAD_ADDR)
    return false;
  Register Addr = LA.getOperand(0).getReg();
  const MachineOperand &SymOp = LA.getOperand(1);
  if (!Addr.isVirtual() || !canAdjustOperand(SymOp))
    return false;

  std::optional<int64_t> Off;
  SmallVector<MachineInstr *, 4> Users;
  for (MachineOperand &MO : MRI.use_nodbg_operands(Addr)) {
    MachineInstr &U = *MO.getParent();
    unsigned BaseIdx;
    if (!memBaseIndex(U.getOpcode(), BaseIdx) ||
        U.getOperandNo(&MO) != BaseIdx || !U.getOperand(2).isImm())
      return false;
    int64_t Imm = U.getOperand(2).getImm();
    if (Off && *Off != Imm)
      return false;
    Off = Imm;
    if (!is_contained(Users, &U))
      Users.push_back(&U);
  }
  if (!Off)
    return false;
  // Each user is a load or store with this address as its base and nowhere
  // else (a store of the address itself was rejected above).
  for (MachineInstr *U : Users) {
    unsigned BaseIdx;
    memBaseIndex(U->getOpcode(), BaseIdx);
    for (unsigned I = 0, E = U->getNumOperands(); I != E; ++I)
      if (I != BaseIdx && U->getOperand(I).isReg() &&
          U->getOperand(I).getReg() == Addr)
        return false;
  }

  MachineOperand HiOp = SymOp;
  if (!adjustOperandOffset(HiOp, *Off))
    return false;
  MachineOperand LoOp = HiOp;
  HiOp.setTargetFlags(SLOW32II::MO_HI);
  LoOp.setTargetFlags(SLOW32II::MO_LO);

  Register Hi = MRI.createVirtualRegister(&SLOW32::GPRRegClass);
  BuildMI(*LA.getParent(), LA, LA.getDebugLoc(), TII->get(SLOW32::LUI), Hi)
      .add(HiOp);

  for (MachineInstr *U : Users) {
    unsigned BaseIdx;
    memBaseIndex(U->getOpcode(), BaseIdx);
    MachineInstrBuilder MIB =
        BuildMI(*U->getParent(), *U, U->getDebugLoc(), TII->get(U->getOpcode()));
    for (unsigned I = 0, E = U->getNumOperands(); I != E; ++I) {
      if (I == BaseIdx)
        MIB.addReg(Hi);
      else if (I == 2)
        MIB.add(LoOp);
      else
        MIB.add(U->getOperand(I));
    }
    MIB.setMemRefs(U->memoperands());
    MIB->setFlags(U->getFlags());
    U->eraseFromParent();
  }
  // Debug values of the full address have nothing left to name.
  for (MachineOperand &MO : make_early_inc_range(MRI.use_operands(Addr)))
    MO.setReg(Register());
  LA.eraseFromParent();
  LLVM_DEBUG(dbgs() << "SLOW32LoadAddrOpt: %lo folded into " << Users.size()
                    << " memory access(es)\n");
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
  // After the addends are in the LOAD_ADDRs: the %lo halves into the
  // loads and stores that use them.
  // (Collected first: the fold rewrites the users, which may be the
  // instructions right after.)
  SmallVector<MachineInstr *, 32> LoadAddrs;
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.getOpcode() == SLOW32::LOAD_ADDR)
        LoadAddrs.push_back(&MI);
  for (MachineInstr *MI : LoadAddrs)
    Changed |= foldLoIntoMemOps(*MI, MRI, TII);

  return Changed;
}

FunctionPass *llvm::createSLOW32LoadAddrOptPass() {
  return new SLOW32LoadAddrOpt();
}
