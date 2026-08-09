//===-- SLOW32FrameLowering.cpp - SLOW32 Frame Information ---------------===//

#include "SLOW32FrameLowering.h"
#include "SLOW32.h"
#include "SLOW32InstrInfo.h"
#include "SLOW32MachineFunctionInfo.h"
#include "SLOW32RegisterInfo.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/CFIInstBuilder.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include <algorithm>
#include <cassert>

using namespace llvm;

namespace {

static const Register StackPtr = SLOW32::R29;
static const Register FramePtr = SLOW32::R30;
static const Register LinkReg = SLOW32::R31;

static bool needsDwarfCFI(const MachineFunction &MF) {
  return MF.needsFrameMoves();
}

static void emitAddImmediateChain(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator InsertPt,
    const DebugLoc &DL, const SLOW32InstrInfo &TII, Register DestReg,
    Register SrcReg, int64_t Amount,
    MachineInstr::MIFlag Flag = MachineInstr::NoFlags) {
  TII.emitStagedAddImmediate(MBB, InsertPt, DL, DestReg, SrcReg, Amount,
                             /*StopWhenImmLegal=*/false, Flag);
}

} // end anonymous namespace

bool SLOW32FrameLowering::hasFPImpl(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  // Need a stable frame pointer when SP moves, the frame is realigned, the
  // address of the frame is taken, or the user disabled FP elimination.
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken() || MFI.hasStackMap() ||
         MFI.hasPatchPoint();
}

bool SLOW32FrameLowering::needsLRSave(const MachineFunction &MF) const {
  // Post-RA the decision is fixed by determineCalleeSaves; before that,
  // fall back to the conservative structural answer.
  if (auto Cached = MF.getInfo<SLOW32MachineFunctionInfo>()->isLRSaved())
    return *Cached;
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  // Calls clobber LR. FRAMEADDR/RETURNADDR and FP setup also require it.
  return MFI.adjustsStack() || MFI.hasCalls() || MFI.isReturnAddressTaken() ||
         hasFP(MF);
}

bool SLOW32FrameLowering::needsFPSave(const MachineFunction &MF) const {
  if (auto Cached = MF.getInfo<SLOW32MachineFunctionInfo>()->isFPSaved())
    return *Cached;
  return hasFP(MF);
}

unsigned
SLOW32FrameLowering::getPrologueSaveSize(const MachineFunction &MF) const {
  unsigned Size = 0;
  if (needsLRSave(MF))
    Size += 4;
  if (needsFPSave(MF))
    Size += 4;
  return Size;
}

uint64_t
SLOW32FrameLowering::getAlignedFrameSize(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  // The datalayout advertises S128: SP must stay 16-byte aligned, so the
  // prologue's single adjustment (locals + fixed saves) is rounded up.
  return alignTo(MFI.getStackSize() + getPrologueSaveSize(MF),
                 getStackAlign().value());
}

void SLOW32FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                               BitVector &SavedRegs,
                                               RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);

  // The fixed prologue saves r30/r31 at SP+0/+4 itself; PEI must not
  // allocate a second CSR spill slot for them.
  SavedRegs.reset(SLOW32::R30);
  SavedRegs.reset(SLOW32::R31);

  // Fix the save decisions now that register allocation is complete. Both
  // registers are allocatable in leaves (r30 only when !hasFP), so the
  // structural triggers alone are not enough: if the allocator used one as
  // a scratch, it must be saved too — r31 still holds the return address
  // at the return, and r30 is call-preserved from the caller's point of
  // view (it is in the CSR regmask).
  auto *FI = MF.getInfo<SLOW32MachineFunctionInfo>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const MachineRegisterInfo &MRI = MF.getRegInfo();
  FI->setLRSaved(MFI.adjustsStack() || MFI.hasCalls() ||
                 MFI.isReturnAddressTaken() || hasFP(MF) ||
                 MRI.isPhysRegModified(SLOW32::R31));
  FI->setFPSaved(hasFP(MF) || MRI.isPhysRegModified(SLOW32::R30));
}

StackOffset
SLOW32FrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                            Register &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const bool HasFP = hasFP(MF);

  FrameReg = HasFP ? FramePtr : StackPtr;

  // ObjectOffset is relative to the incoming SP (where FP points after a
  // hasFP prologue). SP-relative addressing must add the full allocation so
  // far (PEI locals + fixed LR/FP slots at the bottom).
  int64_t Offset = MFI.getObjectOffset(FI) + MFI.getOffsetAdjustment();
  if (!HasFP)
    Offset += static_cast<int64_t>(getAlignedFrameSize(MF));

  return StackOffset::getFixed(Offset);
}

void SLOW32FrameLowering::emitPrologue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  const SLOW32InstrInfo *TII =
      static_cast<const SLOW32InstrInfo *>(MF.getSubtarget().getInstrInfo());

  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc DL;

  assert(SLOW32::GPRRegClass.contains(StackPtr) && "SP not in GPR");
  assert(SLOW32::GPRRegClass.contains(FramePtr) && "FP not in GPR");
  assert(SLOW32::GPRRegClass.contains(LinkReg) && "LR not in GPR");

  const bool HasFP = hasFP(MF);
  const bool SaveLR = needsLRSave(MF);
  const bool SaveFP = needsFPSave(MF);
  const uint64_t FrameSize = getAlignedFrameSize(MF);

  if (FrameSize == 0)
    return;

  // Mark reserved regs live into the entry block.
  if (&MBB == &MF.front()) {
    MachineRegisterInfo &MRI = MF.getRegInfo();
    for (Register R : {StackPtr, LinkReg}) {
      if (!MRI.isLiveIn(R))
        MRI.addLiveIn(R);
      if (!MBB.isLiveIn(R))
        MBB.addLiveIn(R);
    }
    if (SaveFP) {
      if (!MRI.isLiveIn(FramePtr))
        MRI.addLiveIn(FramePtr);
      if (!MBB.isLiveIn(FramePtr))
        MBB.addLiveIn(FramePtr);
    }
  }

  // sp = sp - FrameSize
  emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, StackPtr,
                        -static_cast<int64_t>(FrameSize),
                        MachineInstr::FrameSetup);

  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
    CFIBuilder.buildDefCFAOffset(FrameSize);
  }

  // Layout at the bottom of the frame (low addresses):
  //   SP+0 : LR  (if saved)
  //   SP+4 : old FP (if HasFP; only when LR is also saved, else SP+0)
  //   SP+SaveSize .. : PEI locals
  //   FP = SP+FrameSize = incoming SP
  //
  // Outgoing stack args use real ADJCALLSTACKDOWN (hasReservedCallFrame is
  // false), which temporarily lowers SP below these save slots.
  if (SaveLR) {
    BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
        .addReg(StackPtr)
        .addReg(LinkReg)
        .addImm(0)
        .setMIFlag(MachineInstr::FrameSetup);

    if (needsDwarfCFI(MF)) {
      CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
      CFIBuilder.buildOffset(LinkReg, -static_cast<int64_t>(FrameSize));
    }
  }

  if (SaveFP) {
    const int64_t FPStoreOff = SaveLR ? 4 : 0;
    BuildMI(MBB, MBBI, DL, TII->get(SLOW32::STW))
        .addReg(StackPtr)
        .addReg(FramePtr)
        .addImm(FPStoreOff)
        .setMIFlag(MachineInstr::FrameSetup);

    if (needsDwarfCFI(MF)) {
      CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
      CFIBuilder.buildOffset(FramePtr,
                             -(static_cast<int64_t>(FrameSize) - FPStoreOff));
    }
  }

  if (HasFP) {
    // fp = sp + FrameSize  (points at incoming SP)
    emitAddImmediateChain(MBB, MBBI, DL, *TII, FramePtr, StackPtr,
                          static_cast<int64_t>(FrameSize),
                          MachineInstr::FrameSetup);

    if (needsDwarfCFI(MF)) {
      CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameSetup);
      CFIBuilder.buildDefCFA(FramePtr, 0);
    }
  }
}

void SLOW32FrameLowering::emitEpilogue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const SLOW32InstrInfo *TII =
      static_cast<const SLOW32InstrInfo *>(MF.getSubtarget().getInstrInfo());

  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  DebugLoc DL = MBBI->getDebugLoc();

  const bool HasFP = hasFP(MF);
  const bool SaveLR = needsLRSave(MF);
  const bool SaveFP = needsFPSave(MF);
  const uint64_t FrameSize = getAlignedFrameSize(MF);

  if (FrameSize == 0)
    return;

  // A dynamic alloca leaves SP below the static frame at the return, so
  // recompute it from FP (which points at the incoming SP) before touching
  // the save slots at SP+0/+4.
  if (MFI.hasVarSizedObjects()) {
    assert(HasFP && "variable-sized objects require a frame pointer");
    emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, FramePtr,
                          -static_cast<int64_t>(FrameSize),
                          MachineInstr::FrameDestroy);
  }

  // Before restoring, CFA must be SP-relative again if we had switched to FP.
  if (HasFP && needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildDefCFA(StackPtr, FrameSize);
  }

  if (SaveLR) {
    BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), LinkReg)
        .addReg(StackPtr)
        .addImm(0)
        .setMIFlag(MachineInstr::FrameDestroy);

    if (needsDwarfCFI(MF)) {
      CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
      CFIBuilder.buildRestore(LinkReg);
    }
  }

  if (SaveFP) {
    const int64_t FPStoreOff = SaveLR ? 4 : 0;
    BuildMI(MBB, MBBI, DL, TII->get(SLOW32::LDW), FramePtr)
        .addReg(StackPtr)
        .addImm(FPStoreOff)
        .setMIFlag(MachineInstr::FrameDestroy);

    if (needsDwarfCFI(MF)) {
      CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
      CFIBuilder.buildRestore(FramePtr);
    }
  }

  emitAddImmediateChain(MBB, MBBI, DL, *TII, StackPtr, StackPtr,
                        static_cast<int64_t>(FrameSize),
                        MachineInstr::FrameDestroy);

  if (needsDwarfCFI(MF)) {
    CFIInstBuilder CFIBuilder(MBB, MBBI, MachineInstr::FrameDestroy);
    CFIBuilder.buildDefCFAOffset(0);
  }
}

MachineBasicBlock::iterator SLOW32FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  const SLOW32InstrInfo *TII =
      static_cast<const SLOW32InstrInfo *>(MF.getSubtarget().getInstrInfo());

  if (!hasReservedCallFrame(MF)) {
    // Keep SP 16-byte aligned across the call boundary too: the callee's
    // prologue math assumes an aligned incoming SP.
    int64_t Amount = alignSPAdjust(I->getOperand(0).getImm());
    if (Amount != 0) {
      if (I->getOpcode() == SLOW32::ADJCALLSTACKDOWN) {
        emitAddImmediateChain(MBB, I, I->getDebugLoc(), *TII, StackPtr,
                              StackPtr, -Amount);
      } else {
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
  // Include fixed prologue saves so SP-relative emergency slots stay in range.
  int64_t Estimate = static_cast<int64_t>(MFI.estimateStackSize(MF)) +
                     static_cast<int64_t>(getPrologueSaveSize(MF));
  if (!isInt<11>(Estimate)) {
    int FI = MFI.CreateSpillStackObject(4, Align(4));
    RS->addScavengingFrameIndex(FI);
  }
}
