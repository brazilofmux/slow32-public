//===-- SLOW32RegisterInfo.h - SLOW32 Register Information ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32REGISTERINFO_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32REGISTERINFO_H

#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "SLOW32GenRegisterInfo.inc"

namespace llvm {

class SLOW32RegisterInfo : public SLOW32GenRegisterInfo {
public:
  SLOW32RegisterInfo();

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;
  
  BitVector getReservedRegs(const MachineFunction &MF) const override;
  
  bool eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                          unsigned FIOperandNum,
                          RegScavenger *RS = nullptr) const override;
  
  Register getFrameRegister(const MachineFunction &MF) const override;
};

} // end namespace llvm

#endif