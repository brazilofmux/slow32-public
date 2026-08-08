#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32FRAMELOWERING_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32FRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {

class RegScavenger;

class SLOW32FrameLowering : public TargetFrameLowering {
public:
  SLOW32FrameLowering()
      : TargetFrameLowering(StackGrowsDown, Align(16), 0, Align(16)) {}

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  bool hasFPImpl(const MachineFunction &MF) const override;

  /// Always materialise call frames with ADJCALLSTACK* so outgoing stack
  /// arguments sit below the fixed LR/FP save slots at SP+0/+4.
  bool hasReservedCallFrame(const MachineFunction &MF) const override {
    return false;
  }

  /// Always expand ADJCALLSTACK* in PEI. The default also requires hasFP or
  /// reserved call frames; we have neither in leaf-callers with only an LR
  /// save, and without this the pseudos leak into the asm printer as comments.
  bool canSimplifyCallFramePseudos(const MachineFunction &MF) const override {
    return true;
  }

  /// Bytes for fixed LR/FP saves the prologue places at the bottom of the
  /// frame (not counted in MachineFrameInfo::getStackSize()).
  unsigned getPrologueSaveSize(const MachineFunction &MF) const;

  /// True when the function must preserve LR across its body (calls, etc.).
  bool needsLRSave(const MachineFunction &MF) const;

  StackOffset getFrameIndexReference(const MachineFunction &MF, int FI,
                                     Register &FrameReg) const override;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const override;

  void processFunctionBeforeFrameFinalized(
      MachineFunction &MF, RegScavenger *RS = nullptr) const override;
};

} // namespace llvm

#endif
