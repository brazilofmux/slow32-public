
#include "SLOW32FrameLowering.h"
#include "SLOW32.h"
#include "SLOW32InstrInfo.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"

using namespace llvm;

void SLOW32FrameLowering::emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const SLOW32InstrInfo *TII = 
      static_cast<const SLOW32InstrInfo*>(MF.getSubtarget().getInstrInfo());
  
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc DL;
  
  // Calculate frame size
  uint64_t FrameSize = MFI.getStackSize();
  
  if (FrameSize == 0 && !MFI.adjustsStack())
    return; // No stack frame needed
  
  // Adjust for saving FP and LR (2 * 4 bytes)
  FrameSize = FrameSize + 8;
  
  // Adjust stack pointer: sp = sp - framesize
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::ADDI), SLOW32::SP)
      .addReg(SLOW32::SP)
      .addImm(-FrameSize);
  
  // Save old frame pointer: stw sp+4, fp
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(SLOW32::SP)
      .addReg(SLOW32::FP)
      .addImm(4);
  
  // Save link register: stw sp+0, lr
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(SLOW32::SP)
      .addReg(SLOW32::LR)
      .addImm(0);
  
  // Setup new frame pointer: fp = sp + framesize (point to old fp location)
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::ADDI), SLOW32::FP)
      .addReg(SLOW32::SP)
      .addImm(FrameSize);
}

void SLOW32FrameLowering::emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const SLOW32InstrInfo *TII = 
      static_cast<const SLOW32InstrInfo*>(MF.getSubtarget().getInstrInfo());
  
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  DebugLoc DL = MBBI->getDebugLoc();
  
  uint64_t FrameSize = MFI.getStackSize();
  
  if (FrameSize == 0 && !MFI.adjustsStack())
    return; // No stack frame to restore
  
  // Adjust for saved FP and LR
  FrameSize = FrameSize + 8;
  
  // SP currently points to bottom of frame
  // Restore link register from SP+0
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), SLOW32::LR)
      .addReg(SLOW32::SP)
      .addImm(0);
  
  // Restore old frame pointer from SP+4
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), SLOW32::FP)
      .addReg(SLOW32::SP)
      .addImm(4);
  
  // Restore stack pointer: sp = sp + framesize
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::ADDI), SLOW32::SP)
      .addReg(SLOW32::SP)
      .addImm(FrameSize);
}

MachineBasicBlock::iterator
SLOW32FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  const SLOW32InstrInfo *TII =
      static_cast<const SLOW32InstrInfo*>(MF.getSubtarget().getInstrInfo());
  
  if (!hasReservedCallFrame(MF)) {
    // If we need to adjust stack for the call
    int64_t Amount = I->getOperand(0).getImm();
    
    if (Amount != 0) {
      // Adjust stack pointer
      if (I->getOpcode() == SLOW32::ADJCALLSTACKDOWN) {
        // Allocate space for outgoing arguments
        BuildMI(MBB, I, I->getDebugLoc(), TII->get(SLOW32::ADDI), SLOW32::SP)
            .addReg(SLOW32::SP)
            .addImm(-Amount);
      } else {
        // Deallocate space for outgoing arguments
        BuildMI(MBB, I, I->getDebugLoc(), TII->get(SLOW32::ADDI), SLOW32::SP)
            .addReg(SLOW32::SP)
            .addImm(Amount);
      }
    }
  }
  
  return MBB.erase(I);
}
