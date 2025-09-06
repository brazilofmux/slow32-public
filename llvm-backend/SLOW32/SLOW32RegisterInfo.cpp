//===-- SLOW32RegisterInfo.cpp - SLOW32 Register Information ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the SLOW32 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "SLOW32RegisterInfo.h"
#include "SLOW32.h"
#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_REGINFO_TARGET_DESC
#include "SLOW32GenRegisterInfo.inc"

using namespace llvm;

SLOW32RegisterInfo::SLOW32RegisterInfo() : SLOW32GenRegisterInfo(SLOW32::R31) {}

const MCPhysReg *
SLOW32RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  static const MCPhysReg CalleeSavedRegs[] = {
    SLOW32::R11, SLOW32::R12, SLOW32::R13, SLOW32::R14,
    SLOW32::R15, SLOW32::R16, SLOW32::R17, SLOW32::R18,
    SLOW32::R19, SLOW32::R20, SLOW32::R21, SLOW32::R22,
    SLOW32::R23, SLOW32::R24, SLOW32::R25, SLOW32::R26,
    SLOW32::R27, SLOW32::R28,
    SLOW32::R30, SLOW32::R31,
    0
  };
  return CalleeSavedRegs;
}

BitVector SLOW32RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(SLOW32::R0);   // r0 is always zero
  Reserved.set(SLOW32::R29);  // Stack pointer (sp)
  Reserved.set(SLOW32::R30);  // Frame pointer (fp)
  return Reserved;
}

bool SLOW32RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                              int SPAdj, unsigned FIOperandNum,
                                              RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  
  // Get the frame index operand
  MachineOperand &FIOp = MI.getOperand(FIOperandNum);
  int FrameIndex = FIOp.getIndex();
  
  // Calculate the actual offset from the frame pointer
  int64_t Offset = MFI.getObjectOffset(FrameIndex);
  
  // For ADDI, the frame index should be the second operand (the immediate)
  // The first operand should already be the frame pointer (R30)
  if (MI.getOpcode() == SLOW32::ADDI) {
    // Just change the frame index to the calculated offset
    FIOp.ChangeToImmediate(Offset);
  } else {
    // For other instructions (loads/stores), we may need different handling
    // For now, just change to immediate
    FIOp.ChangeToImmediate(Offset);
  }
  
  return false;
}

Register SLOW32RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return SLOW32::R30;  // FP register
}