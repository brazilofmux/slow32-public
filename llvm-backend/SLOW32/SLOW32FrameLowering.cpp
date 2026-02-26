
#include "SLOW32FrameLowering.h"
#include "SLOW32.h"
#include "SLOW32InstrInfo.h"
#include "SLOW32RegisterInfo.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/CFIInstBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include <algorithm>
#include <cassert>

using namespace llvm;

namespace {

static const Register StackPtr = SLOW32::R29;
static const Register FramePtr = SLOW32::R30;
static const Register LinkReg  = SLOW32::R31;

static bool needsDwarfCFI(const MachineFunction &MF) {
  return MF.needsFrameMoves();
}

static void emitAddImmediateChain(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator InsertPt,
                                  const DebugLoc &DL,
                                  const SLOW32InstrInfo &TII,
                                  Register DestReg, Register SrcReg,
                                  int64_t Amount,
                                  MachineInstr::MIFlag Flag = MachineInstr::NoFlags) {
  if (DestReg != SrcReg) {
    BuildMI(MBB, InsertPt, DL, TII.get(SLOW32::ADD), DestReg)
        .addReg(SrcReg)
        .addReg(SLOW32::R0)
        .setMIFlag(Flag);
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
        .addImm(Step)
        .setMIFlag(Flag);

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
                        -static_cast<int64_t>(FrameSize),
                        MachineInstr::FrameSetup);

  // CFA = SP + FrameSize (SP just moved down)
  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
    CFIBuilder.buildDefCFAOffset(FrameSize);
  }

  // Save old frame pointer: stw sp+4, fp
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(StackPtr)
      .addReg(FramePtr)
      .addImm(4)
      .setMIFlag(MachineInstr::FrameSetup);

  // CFA - (FrameSize - 4) = SP + 4, which is where FP is saved
  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
    CFIBuilder.buildOffset(FramePtr, -(static_cast<int64_t>(FrameSize) - 4));
  }

  // Save link register: stw sp+0, lr
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
      .addReg(StackPtr)
      .addReg(LinkReg)
      .addImm(0)
      .setMIFlag(MachineInstr::FrameSetup);

  // LR saved at CFA - FrameSize = SP + 0
  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
    CFIBuilder.buildOffset(LinkReg, -static_cast<int64_t>(FrameSize));
  }

  // Setup new frame pointer: fp = sp + framesize (point to old fp location)
  emitAddImmediateChain(MBB, MBBI, DL, *TII, FramePtr, StackPtr,
                        static_cast<int64_t>(FrameSize),
                        MachineInstr::FrameSetup);

  // CFA = FP + 0
  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
    CFIBuilder.buildDefCFA(FramePtr, 0);
  }
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
  
  // Switch CFA back to SP-relative before restoring registers
  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildDefCFA(StackPtr, FrameSize);
  }

  // SP currently points to bottom of frame
  // Restore link register from SP+0
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), LinkReg)
      .addReg(StackPtr)
      .addImm(0)
      .setMIFlag(MachineInstr::FrameDestroy);

  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildRestore(LinkReg);
  }

  // Restore old frame pointer from SP+4
  BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), FramePtr)
      .addReg(StackPtr)
      .addImm(4)
      .setMIFlag(MachineInstr::FrameDestroy);

  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildRestore(FramePtr);
  }

  // Restore stack pointer: sp = sp + framesize
  emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, StackPtr,
                        static_cast<int64_t>(FrameSize),
                        MachineInstr::FrameDestroy);

  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildDefCFAOffset(0);
  }
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

void SLOW32FrameLowering::processFunctionBeforeFrameFinalized(
    MachineFunction &MF, RegScavenger *RS) const {
  if (!RS)
    return;

  MachineFrameInfo &MFI = MF.getFrameInfo();

  // estimateStackSize can under-estimate, so use an 11-bit check (2047) to
  // give ourselves a safety margin over the 12-bit signed immediate range.
  if (!isInt<11>(MFI.estimateStackSize(MF))) {
    int FI = MFI.CreateSpillStackObject(4, Align(4));
    RS->addScavengingFrameIndex(FI);
  }
}
