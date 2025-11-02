//===-- SLOW32InstrInfo.cpp - SLOW32 Instruction Information ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32InstrInfo.h"
#include "SLOW32.h"
#include "SLOW32Subtarget.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

#include <iterator>

#define DEBUG_TYPE "slow32-branch"

// Don't include GET_INSTRINFO_ENUM here - it's already in SLOW32.h

#define GET_INSTRINFO_CTOR_DTOR
#include "SLOW32GenInstrInfo.inc"

using namespace llvm;

namespace {
bool isConditionalBranch(unsigned Opcode) {
  switch (Opcode) {
  case SLOW32::BEQ_pat:
  case SLOW32::BNE_pat:
  case SLOW32::BLT:
  case SLOW32::BGE:
  case SLOW32::BGT:
  case SLOW32::BLE:
  case SLOW32::BLTU:
  case SLOW32::BGEU:
  case SLOW32::BGTU:
  case SLOW32::BLEU:
    return true;
  default:
    return false;
  }
}

MachineOperand createMBBOperand(MachineBasicBlock *Dest, unsigned TargetFlag) {
  MachineOperand Op = MachineOperand::CreateMBB(Dest);
  Op.setTargetFlags(TargetFlag);
  return Op;
}

void buildAbsoluteJump(MachineBasicBlock &MBB,
                       MachineBasicBlock::iterator InsertPt, const DebugLoc &DL,
                       const SLOW32InstrInfo &TII, Register ScratchReg,
                       MachineBasicBlock *DestBB) {
  MachineOperand Hi = createMBBOperand(DestBB, SLOW32II::MO_HI);
  MachineOperand Lo = createMBBOperand(DestBB, SLOW32II::MO_LO);

  BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::LUI), ScratchReg).add(Hi);
  BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::ADDI), ScratchReg)
      .addReg(ScratchReg)
      .add(Lo);
  BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::JALR))
      .addReg(SLOW32::R0, RegState::Define)
      .addReg(ScratchReg)
      .addImm(0);
}

// TODO: Add tail duplication guard once we find the correct hook name
// bool SLOW32InstrInfo::isMBBSafeToTailDuplicate(const MachineBasicBlock &MBB) const {
//   // Conservative approach: don't tail-duplicate blocks ending with RET/barrier
//   // This prevents creating invalid MIR with both BR and RET in the same block
//   auto I = MBB.getLastNonDebugInstr();
//   if (I == MBB.end()) 
//     return true; // Empty block is safe
//   
//   // Skip back over any CFI instructions to find the real terminator
//   while (I != MBB.begin() && I->isCFIInstruction())
//     --I;
//   
//   // Don't duplicate blocks ending with return or barrier instructions
//   return !(I->isReturn() || I->isBarrier());
// }

MachineBasicBlock *getBranchTarget(const MachineInstr &MI) {
  assert(MI.getDesc().isBranch() && "Unexpected non-branch opcode");
  unsigned Idx = MI.getNumExplicitOperands();
  if (Idx == 0)
    return nullptr;
  const MachineOperand &MO = MI.getOperand(Idx - 1);
  // NOTE: SLOW32 conditional branch pseudos still use an immediate slot for
  // their offset. Earlier revs blindly called getMBB() here which returned
  // garbage and triggered TailDuplicator asserts when the optimizer tried to
  // rewrite CFG edges. Keep this guard in place (and update tests) if we ever
  // adjust the TableGen patterns.
  if (!MO.isMBB())
    return nullptr;
  return MO.getMBB();
}

void buildCondFromBranch(MachineInstr &MI,
                         SmallVectorImpl<MachineOperand> &Cond) {
  assert(isConditionalBranch(MI.getOpcode()) &&
         "Trying to build a condition from a non-conditional branch");
  Cond.push_back(MachineOperand::CreateImm(MI.getOpcode()));
  Cond.push_back(MI.getOperand(0));
  Cond.push_back(MI.getOperand(1));
}

unsigned getInvertedLongBranchOpcode(unsigned Opcode) {
  switch (Opcode) {
  case SLOW32::PseudoLongBEQ:
    return SLOW32::BNE_pat;
  case SLOW32::PseudoLongBNE:
    return SLOW32::BEQ_pat;
  case SLOW32::PseudoLongBLT:
    return SLOW32::BGE;
  case SLOW32::PseudoLongBGE:
    return SLOW32::BLT;
  case SLOW32::PseudoLongBGT:
    return SLOW32::BLE;
  case SLOW32::PseudoLongBLE:
    return SLOW32::BGT;
  case SLOW32::PseudoLongBLTU:
    return SLOW32::BGEU;
  case SLOW32::PseudoLongBGEU:
    return SLOW32::BLTU;
  case SLOW32::PseudoLongBGTU:
    return SLOW32::BLEU;
  case SLOW32::PseudoLongBLEU:
    return SLOW32::BGTU;
  default:
    llvm_unreachable("Unsupported long branch pseudo");
  }
}

} // namespace

SLOW32InstrInfo::SLOW32InstrInfo(const SLOW32Subtarget &STI)
    : SLOW32GenInstrInfo(STI, SLOW32::ADJCALLSTACKDOWN, SLOW32::ADJCALLSTACKUP,
                         /*CatchRetOpcode=*/0, /*ReturnOpcode=*/0),
      STI(STI) {}

void SLOW32InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator I,
                                   const DebugLoc &DL, Register DestReg,
                                   Register SrcReg, bool KillSrc,
                                   bool RenamableDest,
                                   bool RenamableSrc) const {
  // Use ADD rd, rs, r0 to copy registers
  BuildMI(MBB, I, DL, get(SLOW32::ADD), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc))
      .addReg(SLOW32::R0);
}

void SLOW32InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I,
                                          Register SrcReg, bool isKill,
                                          int FrameIndex,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI,
                                          Register VReg,
                                          MachineInstr::MIFlag Flags) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();
  
  // Store word to stack slot
  BuildMI(MBB, I, DL, get(SLOW32::STW))
      .addReg(SLOW32::R30)  // Frame pointer
      .addReg(SrcReg, getKillRegState(isKill))
      .addFrameIndex(FrameIndex);
}

void SLOW32InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator I,
                                           Register DestReg,
                                           int FrameIndex,
                                           const TargetRegisterClass *RC,
                                           const TargetRegisterInfo *TRI,
                                           Register VReg,
                                           MachineInstr::MIFlag Flags) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();
  
  // Load word from stack slot
  BuildMI(MBB, I, DL, get(SLOW32::LDW), DestReg)
      .addReg(SLOW32::R30)  // Frame pointer
      .addFrameIndex(FrameIndex);
}

// Analyze the branching code at the end of MBB
bool SLOW32InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  TBB = nullptr;
  FBB = nullptr;
  Cond.clear();

  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return false;

  while (I->isCFIInstruction() && I != MBB.begin())
    --I;

  if (!isUnpredicatedTerminator(*I))
    return false;

  MachineBasicBlock::iterator FirstUncond = MBB.end();
  int NumTerminators = 0;
  for (auto It = I.getReverse(); It != MBB.rend() && isUnpredicatedTerminator(*It);
       ++It) {
    ++NumTerminators;
    if (It->getDesc().isUnconditionalBranch() ||
        It->getDesc().isIndirectBranch())
      FirstUncond = It.getReverse();
  }

  if (AllowModify && FirstUncond != MBB.end()) {
    while (std::next(FirstUncond) != MBB.end()) {
      std::next(FirstUncond)->eraseFromParent();
      --NumTerminators;
    }
    I = FirstUncond;
  }

  if (I->getDesc().isIndirectBranch() || I->isPreISelOpcode())
    return true;

  if (NumTerminators > 2)
    return true;

  if (NumTerminators == 1) {
    if (I->getDesc().isUnconditionalBranch()) {
      if (MachineBasicBlock *Target = getBranchTarget(*I)) {
        TBB = Target;
        return false;
      }
      return true;
    }
    if (I->getDesc().isConditionalBranch()) {
      MachineBasicBlock *Target = getBranchTarget(*I);
      if (!Target)
        return true;
      buildCondFromBranch(*I, Cond);
      TBB = Target;
      return false;
    }
    return true;
  }

  // NumTerminators == 2
  MachineBasicBlock::iterator SecondLast = std::prev(I);
  while (SecondLast->isCFIInstruction() || SecondLast->isDebugInstr()) {
    if (SecondLast == MBB.begin())
      break;
    --SecondLast;
  }

  if (!SecondLast->getDesc().isConditionalBranch() ||
      !I->getDesc().isUnconditionalBranch())
    return true;

  MachineBasicBlock *TrueTarget = getBranchTarget(*SecondLast);
  MachineBasicBlock *FalseTarget = getBranchTarget(*I);
  if (!TrueTarget || !FalseTarget)
    return true;

  buildCondFromBranch(*SecondLast, Cond);
  TBB = TrueTarget;
  FBB = FalseTarget;
  return false;
}

unsigned SLOW32InstrInfo::insertBranch(MachineBasicBlock &MBB,
                                       MachineBasicBlock *TBB,
                                       MachineBasicBlock *FBB,
                                       ArrayRef<MachineOperand> Cond,
                                       const DebugLoc &DL,
                                       int *BytesAdded) const {
  assert(TBB && "insertBranch must have a target basic block");

  if (BytesAdded)
    *BytesAdded = 0;

  if (Cond.empty()) {
    MachineInstr &MI = *BuildMI(&MBB, DL, get(SLOW32::BR)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI);
    return 1;
  }

  assert(Cond.size() == 3 && "Invalid condition");

  MachineInstr &CondMI =
      *BuildMI(&MBB, DL, get(Cond[0].getImm()))
            .add(Cond[1])
            .add(Cond[2])
            .addMBB(TBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(CondMI);

  if (!FBB)
    return 1;

  MachineInstr &BrMI = *BuildMI(&MBB, DL, get(SLOW32::BR)).addMBB(FBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(BrMI);
  return 2;
}

unsigned SLOW32InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                       int *BytesRemoved) const {
  if (BytesRemoved)
    *BytesRemoved = 0;

  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return 0;

  if (!I->getDesc().isUnconditionalBranch() &&
      !I->getDesc().isConditionalBranch())
    return 0;

  if (BytesRemoved)
    *BytesRemoved += getInstSizeInBytes(*I);
  I->eraseFromParent();

  I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return 1;

  if (!I->getDesc().isConditionalBranch())
    return 1;

  if (BytesRemoved)
    *BytesRemoved += getInstSizeInBytes(*I);
  I->eraseFromParent();
  return 2;
}

bool SLOW32InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 3 && "Invalid condition");
  
  unsigned Opcode = Cond[0].getImm();
  unsigned NewOpcode;
  
  // Reverse the condition
  switch (Opcode) {
  case SLOW32::BEQ_pat:
    NewOpcode = SLOW32::BNE_pat;
    break;
  case SLOW32::BNE_pat:
    NewOpcode = SLOW32::BEQ_pat;
    break;
  case SLOW32::BLT:
    NewOpcode = SLOW32::BGE;
    break;
  case SLOW32::BGE:
    NewOpcode = SLOW32::BLT;
    break;
  case SLOW32::BGT:
    NewOpcode = SLOW32::BLE;
    break;
  case SLOW32::BLE:
    NewOpcode = SLOW32::BGT;
    break;
  case SLOW32::BLTU:
    NewOpcode = SLOW32::BGEU;
    break;
  case SLOW32::BGEU:
    NewOpcode = SLOW32::BLTU;
    break;
  case SLOW32::BGTU:
    NewOpcode = SLOW32::BLEU;
    break;
  case SLOW32::BLEU:
    NewOpcode = SLOW32::BGTU;
    break;
  default:
    return true; // Can't reverse this condition
  }
  
  Cond[0].setImm(NewOpcode);
  return false;
}

bool SLOW32InstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  auto PropagateFlags = [&](MachineInstr &NewMI) {
    NewMI.setFlags(NewMI.getFlags() | MI.getFlags());
  };

  switch (MI.getOpcode()) {
  case SLOW32::LOAD_ADDR: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();
    Register DestReg = MI.getOperand(0).getReg();
    const MachineOperand &Symbol = MI.getOperand(1);

    MachineInstrBuilder HiMI =
        BuildMI(MBB, MI, DL, get(SLOW32::LUI), DestReg).add(Symbol);
    HiMI->getOperand(1).setTargetFlags(SLOW32II::MO_HI);
    PropagateFlags(*HiMI);

    MachineInstrBuilder LoMI =
        BuildMI(MBB, MI, DL, get(SLOW32::ADDI), DestReg)
            .addReg(DestReg)
            .add(Symbol);
    LoMI->getOperand(2).setTargetFlags(SLOW32II::MO_LO);
    PropagateFlags(*LoMI);

    MI.eraseFromParent();
    return true;
  }
  case SLOW32::JAL_CALL: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();

    MachineInstrBuilder MIB =
        BuildMI(MBB, MI, DL, get(SLOW32::JAL)).add(MI.getOperand(0));

    for (unsigned I = 1, E = MI.getNumOperands(); I < E; ++I)
      MIB.add(MI.getOperand(I));

    PropagateFlags(*MIB);
    for (MachineMemOperand *MMO : MI.memoperands())
      MIB.addMemOperand(MMO);

    MI.eraseFromParent();
    return true;
  }
  case SLOW32::JALR_CALL: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();

    MachineInstrBuilder MIB =
        BuildMI(MBB, MI, DL, get(SLOW32::JALR))
            .addReg(SLOW32::R31, RegState::Define);

    for (const MachineOperand &MO : MI.operands())
      MIB.add(MO);

    PropagateFlags(*MIB);
    for (MachineMemOperand *MMO : MI.memoperands())
      MIB.addMemOperand(MMO);

    MI.eraseFromParent();
    return true;
  }
  case SLOW32::BRIND_JALR: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();

    MachineInstrBuilder MIB =
        BuildMI(MBB, MI, DL, get(SLOW32::JALR))
            .addReg(SLOW32::R0, RegState::Define);

    for (const MachineOperand &MO : MI.operands())
      MIB.add(MO);

    PropagateFlags(*MIB);
    for (MachineMemOperand *MMO : MI.memoperands())
      MIB.addMemOperand(MMO);

    MI.eraseFromParent();
    return true;
  }
  case SLOW32::PseudoLongBR: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();
    Register Scratch = MI.getOperand(0).getReg();
    MachineBasicBlock *Dest = MI.getOperand(1).getMBB();

    auto InsertPos = MI.getIterator();
    buildAbsoluteJump(MBB, InsertPos, DL, *this, Scratch, Dest);

    for (int I = 1; I <= 3; ++I) {
      auto It = std::prev(InsertPos, I);
      PropagateFlags(*It);
    }

    MI.eraseFromParent();
    return true;
  }
  case SLOW32::PseudoLongBEQ:
  case SLOW32::PseudoLongBNE:
  case SLOW32::PseudoLongBLT:
  case SLOW32::PseudoLongBGE:
  case SLOW32::PseudoLongBGT:
  case SLOW32::PseudoLongBLE:
  case SLOW32::PseudoLongBLTU:
  case SLOW32::PseudoLongBGEU:
  case SLOW32::PseudoLongBGTU:
  case SLOW32::PseudoLongBLEU: {
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();
    Register Scratch = MI.getOperand(0).getReg();
    Register LHS = MI.getOperand(1).getReg();
    Register RHS = MI.getOperand(2).getReg();
    MachineBasicBlock *Dest = MI.getOperand(3).getMBB();

    unsigned InvertedOpc = getInvertedLongBranchOpcode(MI.getOpcode());
    MachineInstrBuilder Skip = BuildMI(MBB, MI, DL, get(InvertedOpc))
                                   .addReg(LHS)
                                   .addReg(RHS)
                                   .addImm(12);
    PropagateFlags(*Skip);

    auto InsertPos = MI.getIterator();
    buildAbsoluteJump(MBB, InsertPos, DL, *this, Scratch, Dest);
    for (int I = 1; I <= 3; ++I) {
      auto It = std::prev(InsertPos, I);
      PropagateFlags(*It);
    }

    MI.eraseFromParent();
    return true;
  }
  default:
    return false;
  }
}

bool SLOW32InstrInfo::isBranchOffsetInRange(unsigned BranchOpc,
                                            int64_t Offset) const {
  auto IsEven = [](int64_t Value) { return (Value & 0x1) == 0; };

  switch (BranchOpc) {
  case SLOW32::BEQ:
  case SLOW32::BEQ_pat:
  case SLOW32::BNE:
  case SLOW32::BNE_pat:
  case SLOW32::BLT:
  case SLOW32::BGE:
  case SLOW32::BGT:
  case SLOW32::BLE:
  case SLOW32::BLTU:
  case SLOW32::BGEU:
  case SLOW32::BGTU:
  case SLOW32::BLEU:
    return isInt<13>(Offset) && IsEven(Offset);
  case SLOW32::BR:
  case SLOW32::JAL:
  case SLOW32::JAL_CALL:
    return isInt<21>(Offset) && IsEven(Offset);
  default:
    return true;
  }
}

MachineBasicBlock *
SLOW32InstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();

  // Handle JAL before consulting descriptor flags because it may be marked as
  // a call even when targeting an MBB.
  if (Opcode == SLOW32::JAL) {
    unsigned NumExplicit = MI.getNumExplicitOperands();
    if (NumExplicit == 0)
      return nullptr;
    const MachineOperand &Op = MI.getOperand(0);
#ifndef NDEBUG
    if (!Op.isMBB()) {
      dbgs() << "JAL with non-MBB target (call, not branch): ";
      MI.dump();
    }
#endif
    if (!Op.isMBB())
      return nullptr;
    return Op.getMBB();
  }

  if (!MI.getDesc().isBranch())
    return nullptr;

  // Calls are not branches for the purposes of branch relaxation
  if (MI.isCall())
    return nullptr;

  // JALR and other indirect branches don't have a single destination
  if (MI.getDesc().isIndirectBranch())
    return nullptr;

  // Try the standard getBranchTarget helper first
  if (MachineBasicBlock *Dest = getBranchTarget(MI))
    return Dest;

  // Handle Pseudo long branches explicitly - they always have the target as the last operand
  switch (MI.getOpcode()) {
  case SLOW32::PseudoLongBR:
  case SLOW32::PseudoLongBEQ:
  case SLOW32::PseudoLongBNE:
  case SLOW32::PseudoLongBLT:
  case SLOW32::PseudoLongBGE:
  case SLOW32::PseudoLongBGT:
  case SLOW32::PseudoLongBLE:
  case SLOW32::PseudoLongBLTU:
  case SLOW32::PseudoLongBGEU:
  case SLOW32::PseudoLongBGTU:
  case SLOW32::PseudoLongBLEU: {
    unsigned NumOps = MI.getNumOperands();
    if (NumOps > 0) {
      const MachineOperand &MO = MI.getOperand(NumOps - 1);
      if (MO.isMBB())
        return MO.getMBB();
    }
    break;
  }
  default:
    break;
  }

  // If we got here, we have a direct branch but couldn't find the MBB
  // This can happen for raw instructions (BEQ/BNE with immediates)
  // which should be pre-RA only. During and after RA, all direct branches
  // must use the pattern versions with MBB operands.

  // Debug: dump the instruction that's causing issues
#ifndef NDEBUG
  dbgs() << "WARNING: getBranchDestBlock returning nullptr for: ";
  MI.dump();
#endif

  return nullptr;
}

void SLOW32InstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
                                           MachineBasicBlock &DestBB,
                                           MachineBasicBlock &RestoreBB,
                                           const DebugLoc &DL,
                                           int64_t BrOffset,
                                           RegScavenger *RS) const {
  (void)BrOffset;
  assert(MBB.empty() && "expected a dedicated block for the long branch");
  assert(RestoreBB.empty() && "SLOW32 long branches do not use restore blocks");

  assert(RS && "RegScavenger required for SLOW32 long branches");
  RS->enterBasicBlockEnd(MBB);
  Register ScratchPhys = RS->scavengeRegisterBackwards(SLOW32::GPRRegClass,
                                                       MBB.end(),
                                                       /*RestoreAfter=*/false,
                                                       /*SPAdj=*/0);
  if (!ScratchPhys)
    report_fatal_error("Unable to scavenge register for SLOW32 long branch");

  RS->setRegUsed(ScratchPhys);

  LLVM_DEBUG(dbgs() << "Emitting long branch pseudo with scratch register r"
                    << ScratchPhys << "\n");

  BuildMI(MBB, MBB.end(), DL, get(SLOW32::PseudoLongBR), ScratchPhys)
      .addMBB(&DestBB);
}

unsigned SLOW32InstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case SLOW32::PseudoLongBR:
    return 12; // LUI + ADDI + JALR
  case SLOW32::PseudoLongBEQ:
  case SLOW32::PseudoLongBNE:
  case SLOW32::PseudoLongBLT:
  case SLOW32::PseudoLongBGE:
  case SLOW32::PseudoLongBGT:
  case SLOW32::PseudoLongBLE:
  case SLOW32::PseudoLongBLTU:
  case SLOW32::PseudoLongBGEU:
  case SLOW32::PseudoLongBGTU:
  case SLOW32::PseudoLongBLEU:
    return 16; // inverted compare + long jump sequence
  default:
    break;
  }

  if (MI.isMetaInstruction())
    return 0;

  unsigned Opcode = MI.getOpcode();
  if (Opcode == TargetOpcode::INLINEASM ||
      Opcode == TargetOpcode::INLINEASM_BR) {
    const MachineFunction &MF = *MI.getParent()->getParent();
    return getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                              *MF.getTarget().getMCAsmInfo());
  }

  if (Opcode == TargetOpcode::BUNDLE) {
    unsigned Size = 0;
    auto I = MI.getIterator();
    auto E = MI.getParent()->instr_end();
    while (++I != E && I->isInsideBundle()) {
      assert(!I->isBundle() && "Nested bundles are not supported");
      Size += getInstSizeInBytes(*I);
    }
    return Size;
  }

  return MI.getDesc().getSize();
}

// TODO: Add tail duplication guard once we find the correct hook name
// bool SLOW32InstrInfo::isMBBSafeToTailDuplicate(const MachineBasicBlock &MBB) const {
//   // Conservative approach: don't tail-duplicate blocks ending with RET/barrier
//   // This prevents creating invalid MIR with both BR and RET in the same block
//   auto I = MBB.getLastNonDebugInstr();
//   if (I == MBB.end())
//     return true; // Empty block is safe
//   // Skip back over any CFI instructions to find the real terminator
//   while (I != MBB.begin() && I->isCFIInstruction())
//     --I;
//   // Don't duplicate blocks ending with return or barrier instructions
//   return !(I->isReturn() || I->isBarrier());
// }
