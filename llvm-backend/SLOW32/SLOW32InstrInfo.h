//===-- SLOW32InstrInfo.h - SLOW32 Instruction Information ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32INSTRINFO_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32INSTRINFO_H

#include "SLOW32RegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "SLOW32GenInstrInfo.inc"

namespace llvm {

class SLOW32InstrInfo : public SLOW32GenInstrInfo {
public:
  SLOW32InstrInfo();

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                   const DebugLoc &DL, Register DestReg, Register SrcReg,
                   bool KillSrc, bool RenamableDest = false,
                   bool RenamableSrc = false) const override;
  
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator I,
                          Register SrcReg, bool isKill, int FrameIndex,
                          const TargetRegisterClass *RC,
                          const TargetRegisterInfo *TRI,
                          Register VReg,
                          MachineInstr::MIFlag Flags = MachineInstr::NoFlags) const override;
  
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator I,
                           Register DestReg, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI,
                           Register VReg,
                           MachineInstr::MIFlag Flags = MachineInstr::NoFlags) const override;

  // Branch analysis
  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                      MachineBasicBlock *&FBB,
                      SmallVectorImpl<MachineOperand> &Cond,
                      bool AllowModify = false) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                         MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                         const DebugLoc &DL,
                         int *BytesAdded = nullptr) const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  bool reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  // Pseudo instruction expansion
  bool expandPostRAPseudo(MachineInstr &MI) const override;
};

} // end namespace llvm

#endif