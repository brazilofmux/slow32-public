//===-- SLOW32.h - Top-level interface for SLOW32 --------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// SLOW32 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32_H

#include "llvm/Target/TargetMachine.h"

namespace llvm {
class FunctionPass;
class SLOW32TargetMachine;

FunctionPass *createSLOW32ISelDag(SLOW32TargetMachine &TM);
// FunctionPass *createSLOW32NormalizeCallTailsPass();  // Disabled - not needed after upstream fixes
// FunctionPass *createSLOW32RepairCFGFromTerminatorsPass();  // Disabled - not needed after upstream fixes

// SLOW32 specific instruction flags
namespace SLOW32II {
enum TargetFlags {
  MO_None = 0,
  MO_HI = 1,  // %hi(symbol) - upper 20 bits
  MO_LO = 2   // %lo(symbol) - lower 12 bits
};
} // end namespace SLOW32II

} // end namespace llvm

// Include generated enums - this provides register and instruction definitions
#define GET_REGINFO_ENUM
#include "SLOW32GenRegisterInfo.inc"

#define GET_INSTRINFO_ENUM  
#include "SLOW32GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "SLOW32GenSubtargetInfo.inc"

#endif