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

class SLOW32Subtarget;
class RegScavenger;

class SLOW32InstrInfo : public SLOW32GenInstrInfo {
  const SLOW32Subtarget &STI;

public:
  explicit SLOW32InstrInfo(const SLOW32Subtarget &STI);

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                   const DebugLoc &DL, Register DestReg, Register SrcReg,
                   bool KillSrc, bool RenamableDest = false,
                   bool RenamableSrc = false) const override;
  
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator I, Register SrcReg,
                           bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC, Register VReg,
                           MachineInstr::MIFlag Flags =
                               MachineInstr::NoFlags) const override;

  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator I, Register DestReg,
                            int FrameIndex, const TargetRegisterClass *RC,
                            Register VReg, unsigned SubReg = 0,
                            MachineInstr::MIFlag Flags =
                                MachineInstr::NoFlags) const override;

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

  bool isBranchOffsetInRange(unsigned BranchOpc, int64_t Offset) const override;

  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const override;

  void insertIndirectBranch(MachineBasicBlock &MBB, MachineBasicBlock &DestBB,
                            MachineBasicBlock &RestoreBB, const DebugLoc &DL,
                            int64_t BrOffset,
                            RegScavenger *RS) const override;

  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  /// Emit `DestReg = SrcReg + Amount` as a staged chain of legal 12-bit
  /// ADDI steps (with an initial copy when DestReg != SrcReg). With
  /// \p StopWhenImmLegal the chain stops as soon as the remainder fits a
  /// signed 12-bit immediate and returns it (for absorption into a memory
  /// operand's displacement); otherwise the chain runs to zero. This is
  /// the single implementation shared by frame lowering (SP adjustment)
  /// and frame-index elimination (base materialisation).
  int64_t emitStagedAddImmediate(
      MachineBasicBlock &MBB, MachineBasicBlock::iterator InsertPt,
      const DebugLoc &DL, Register DestReg, Register SrcReg, int64_t Amount,
      bool StopWhenImmLegal = false,
      MachineInstr::MIFlag Flag = MachineInstr::NoFlags) const;
};

} // end namespace llvm

#endif
