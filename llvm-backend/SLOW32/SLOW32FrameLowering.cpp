
#include "SLOW32FrameLowering.h"
#include "SLOW32.h"
#include "SLOW32InstrInfo.h"
#include "SLOW32RegisterInfo.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include <algorithm>
#include <cassert>

using namespace llvm;

namespace {

static const Register StackPtr = SLOW32::R29;
static const Register FramePtr = SLOW32::R30;
static const Register LinkReg  = SLOW32::R31;

static void emitAddImmediateChain(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator InsertPt,
                                  const DebugLoc &DL,
                                  const SLOW32InstrInfo &TII,
                                  Register DestReg, Register SrcReg,
                                  int64_t Amount) {
  if (DestReg != SrcReg) {
    BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::ADD), DestReg)
        .addReg(SrcReg)
        .addReg(SLOW32::R0);
  }

  Register CurrReg = DestReg;
  int64_t Remaining = Amount;

  while (Remaining != 0) {
    int64_t Step;
    if (Remaining > 0)
      Step = std::min<int64_t>(Remaining, 2047);
    else
      Step = std::max<int64_t>(Remaining, -2048);

    BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::ADDI), CurrReg)
        .addReg(CurrReg)
        .addImm(Step);

    Remaining -= Step;
  }
}

} // end anonymous namespace

void SLOW32FrameLowering::emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const SLOW32InstrInfo *TII =
      static_cast<const SLOW32InstrInfo*>(MF.getSubtarget().getInstrInfo());

  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc DL;

  assert(SLOW32::GPRRegClass.contains(StackPtr) && "SP not in GPR");
  assert(SLOW32::GPRRegClass.contains(FramePtr) && "FP not in GPR");
  assert(SLOW32::GPRRegClass.contains(LinkReg) && "LR not in GPR");

  if (&MBB == &MF.front()) {
    MachineRegisterInfo &MRI = MF.getRegInfo();
    for (Register R : {StackPtr, FramePtr, LinkReg}) {
      if (!MRI.isLiveIn(R))
        MRI.addLiveIn(R);
      if (!MBB.isLiveIn(R))
        MBB.addLiveIn(R);
    }
  }
  
  // Calculate frame size
  uint64_t FrameSize = MFI.getStackSize();
  
  if (FrameSize == 0 && !MFI.adjustsStack())
    return; // No stack frame needed
  
  // Adjust for saving FP and LR (2 * 4 bytes)
  FrameSize = FrameSize + 8;
  
  // Adjust stack pointer: sp = sp - framesize (may require multiple steps)
  emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, StackPtr,
                        -static_cast<int64_t>(FrameSize));

  // Save old frame pointer: stw sp+4, fp
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(StackPtr)
      .addReg(FramePtr)
      .addImm(4);

  // Save link register: stw sp+0, lr
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(StackPtr)
      .addReg(LinkReg)
      .addImm(0);

  // Setup new frame pointer: fp = sp + framesize (point to old fp location)
  emitAddImmediateChain(MBB, MBBI, DL, *TII, FramePtr, StackPtr,
                        static_cast<int64_t>(FrameSize));
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
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), LinkReg)
      .addReg(StackPtr)
      .addImm(0);

  // Restore old frame pointer from SP+4
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), FramePtr)
      .addReg(StackPtr)
      .addImm(4);

  // Restore stack pointer: sp = sp + framesize
  emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, StackPtr,
                        static_cast<int64_t>(FrameSize));
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
        emitAddImmediateChain(MBB, I, I->getDebugLoc(), *TII, StackPtr,
                              StackPtr, -Amount);
      } else {
        // Deallocate space for outgoing arguments
        emitAddImmediateChain(MBB, I, I->getDebugLoc(), *TII, StackPtr,
                              StackPtr, Amount);
      }
    }
  }
  
  return MBB.erase(I);
}
