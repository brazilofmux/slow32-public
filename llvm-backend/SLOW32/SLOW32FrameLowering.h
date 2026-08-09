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

  /// True when the function must preserve LR across its body: calls, FP
  /// setup, or (post-RA) the allocator having used r31 as a scratch.
  bool needsLRSave(const MachineFunction &MF) const;

  /// True when the prologue must save r30: either it serves as the frame
  /// pointer, or (post-RA) the allocator used it as a scratch and the
  /// caller may rely on it being preserved (r30 is in the CSR regmask).
  bool needsFPSave(const MachineFunction &MF) const;

  /// Full SP adjustment the prologue performs: locals + fixed saves,
  /// rounded up to the 16-byte stack alignment the datalayout advertises.
  uint64_t getAlignedFrameSize(const MachineFunction &MF) const;

  /// Fixes the LR/FP save decisions (see SLOW32MachineFunctionInfo) and
  /// keeps r30/r31 out of the generic CSR spill machinery — the fixed
  /// prologue saves them itself.
  void determineCalleeSaves(MachineFunction &MF, BitVector &SavedRegs,
                            RegScavenger *RS) const override;

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
