#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32FRAMELOWERING_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32FRAMELOWERING_H
#include "llvm/CodeGen/TargetFrameLowering.h"
namespace llvm {
class SLOW32FrameLowering : public TargetFrameLowering {
public:
  SLOW32FrameLowering() : TargetFrameLowering(StackGrowsDown, Align(16), 0, Align(16)) {}
  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  bool hasFPImpl(const MachineFunction &MF) const override { return true; }
  
  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const override;
};
}
#endif