//===-- SLOW32InstrInfo.cpp - SLOW32 Instruction Information ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32InstrInfo.h"
#include "SLOW32.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "slow32-branch"

// Don't include GET_INSTRINFO_ENUM here - it's already in SLOW32.h

#define GET_INSTRINFO_CTOR_DTOR
#include "SLOW32GenInstrInfo.inc"

using namespace llvm;

SLOW32InstrInfo::SLOW32InstrInfo()
    : SLOW32GenInstrInfo(SLOW32::ADJCALLSTACKDOWN, SLOW32::ADJCALLSTACKUP) {}

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
  LLVM_DEBUG(dbgs() << "SLOW32: analyzeBranch for BB#" << MBB.getNumber() << "\n");
  
  // Start from the end of the basic block
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return false;
  
  LLVM_DEBUG(dbgs() << "  Last non-debug instr opcode: " << I->getOpcode() 
                    << " (BEQ_pat=" << SLOW32::BEQ_pat << ", BNE_pat=" << SLOW32::BNE_pat
                    << ", BR=" << SLOW32::BR << ")\n");

  // Check for unconditional branch (BR)
  if (I->getOpcode() == SLOW32::BR) {
    LLVM_DEBUG(dbgs() << "  Last instr is BR\n");
    if (!I->getOperand(0).isMBB())
      return true; // Can't handle indirect branch
    
    // Check if there's a conditional branch before this unconditional branch
    if (I != MBB.begin()) {
      MachineBasicBlock::iterator J = I;
      --J;
      while (J->isDebugInstr() && J != MBB.begin())
        --J;
        
      unsigned PrevOpcode = J->getOpcode();
      if (PrevOpcode == SLOW32::BEQ_pat || PrevOpcode == SLOW32::BNE_pat ||
          PrevOpcode == SLOW32::BLT || PrevOpcode == SLOW32::BGE) {
        // Conditional branch followed by unconditional branch
        LLVM_DEBUG(dbgs() << "  Found cond before uncond, opcode=" << PrevOpcode << "\n");
        if (J->getOperand(2).isMBB()) {
          TBB = J->getOperand(2).getMBB(); // Conditional target
          FBB = I->getOperand(0).getMBB(); // Unconditional target
          
          // Save the condition
          Cond.push_back(MachineOperand::CreateImm(PrevOpcode));
          Cond.push_back(J->getOperand(0)); // First register
          Cond.push_back(J->getOperand(1)); // Second register
          return false;
        }
      }
    }
    
    // Just an unconditional branch
    TBB = I->getOperand(0).getMBB();
    return false;
  }

  // Check for conditional branches
  unsigned Opcode = I->getOpcode();
  if (Opcode == SLOW32::BEQ_pat || Opcode == SLOW32::BNE_pat ||
      Opcode == SLOW32::BLT || Opcode == SLOW32::BGE) {
    LLVM_DEBUG(dbgs() << "  Last instr is conditional branch opcode=" << Opcode << "\n");
    // Conditional branch
    MachineBasicBlock *TargetBB = nullptr;
    if (I->getOperand(2).isMBB()) {
      TargetBB = I->getOperand(2).getMBB();
    } else {
      return true; // Can't handle this branch
    }

    // Check if there's a following unconditional branch
    MachineBasicBlock::iterator J = I;
    ++J;
    while (J != MBB.end() && J->isDebugInstr())
      ++J;
    
    if (J != MBB.end() && J->getOpcode() == SLOW32::BR && J->getOperand(0).isMBB()) {
      // Conditional branch followed by unconditional branch
      TBB = TargetBB;
      FBB = J->getOperand(0).getMBB();
      
      // Save the condition
      Cond.push_back(MachineOperand::CreateImm(Opcode));
      Cond.push_back(I->getOperand(0)); // First register
      Cond.push_back(I->getOperand(1)); // Second register
      
      LLVM_DEBUG(dbgs() << "  Found cond+uncond: TBB=BB#" << TBB->getNumber() 
                        << " FBB=BB#" << FBB->getNumber() << "\n");
      return false;
    }
    
    // Just a conditional branch with fall-through
    TBB = TargetBB;
    LLVM_DEBUG(dbgs() << "  Found cond only: TBB=BB#" << TBB->getNumber() << "\n");
    
    // Save the condition
    Cond.push_back(MachineOperand::CreateImm(Opcode));
    Cond.push_back(I->getOperand(0)); // First register
    Cond.push_back(I->getOperand(1)); // Second register
    
    return false;
  }

  // Unknown branch type
  return true;
}

unsigned SLOW32InstrInfo::insertBranch(MachineBasicBlock &MBB,
                                        MachineBasicBlock *TBB,
                                        MachineBasicBlock *FBB,
                                        ArrayRef<MachineOperand> Cond,
                                        const DebugLoc &DL,
                                        int *BytesAdded) const {
  assert(TBB && "insertBranch must have a target basic block");
  
  unsigned Count = 0;
  
  // Unconditional branch
  if (Cond.empty()) {
    BuildMI(&MBB, DL, get(SLOW32::BR)).addMBB(TBB);
    Count = 1;
  } else {
    // Conditional branch
    assert(Cond.size() == 3 && "Invalid condition");
    unsigned Opcode = Cond[0].getImm();
    
    BuildMI(&MBB, DL, get(Opcode))
        .add(Cond[1])  // First register
        .add(Cond[2])  // Second register
        .addMBB(TBB);
    Count = 1;
    
    // Add unconditional branch if FBB is specified
    if (FBB) {
      BuildMI(&MBB, DL, get(SLOW32::BR)).addMBB(FBB);
      Count = 2;
    }
  }
  
  if (BytesAdded)
    *BytesAdded = Count * 4; // Each instruction is 4 bytes
    
  return Count;
}

unsigned SLOW32InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                        int *BytesRemoved) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return 0;
  
  unsigned Count = 0;
  
  // Remove branches from the end
  while (I != MBB.begin()) {
    unsigned Opcode = I->getOpcode();
    if (Opcode != SLOW32::BR && 
        Opcode != SLOW32::BEQ_pat && 
        Opcode != SLOW32::BNE_pat &&
        Opcode != SLOW32::BLT &&
        Opcode != SLOW32::BGE)
      break;
    
    // Remove this branch
    MachineBasicBlock::iterator J = I;
    --I;
    J->eraseFromParent();
    Count++;
    
    // Stop after removing 2 branches (conditional + unconditional)
    if (Count == 2)
      break;
  }
  
  if (BytesRemoved)
    *BytesRemoved = Count * 4;
    
  return Count;
}

bool SLOW32InstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case SLOW32::LOAD_ADDR: {
    // Expand LOAD_ADDR pseudo into LUI + ORI sequence
    // This ensures they stay together in the correct order
    MachineBasicBlock &MBB = *MI.getParent();
    const DebugLoc &DL = MI.getDebugLoc();
    Register DestReg = MI.getOperand(0).getReg();
    const MachineOperand &Symbol = MI.getOperand(1);
    
    // Generate LUI rd, %hi(symbol)
    BuildMI(MBB, MI, DL, get(SLOW32::LUI), DestReg)
      .add(Symbol);  // Will be printed with %hi
    
    // Generate ORI rd, rd, %lo(symbol)  
    BuildMI(MBB, MI, DL, get(SLOW32::ORI), DestReg)
      .addReg(DestReg)
      .add(Symbol);  // Will be printed with %lo
    
    MI.eraseFromParent();
    return true;
  }
  default:
    return false;
  }
}

bool SLOW32InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 3 && "Invalid condition");
  
  unsigned Opcode = Cond[0].getImm();
  unsigned NewOpcode;
  
  // Reverse the condition
  switch (Opcode) {
  case SLOW32::BEQ:
    NewOpcode = SLOW32::BNE_pat;
    break;
  case SLOW32::BNE_pat:
    NewOpcode = SLOW32::BEQ;
    break;
  case SLOW32::BLT:
    NewOpcode = SLOW32::BGE;
    break;
  case SLOW32::BGE:
    NewOpcode = SLOW32::BLT;
    break;
  default:
    return true; // Can't reverse this condition
  }
  
  Cond[0].setImm(NewOpcode);
  return false;
}