//===-- SLOW32Subtarget.cpp - SLOW32 Subtarget Information --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32Subtarget.h"
#include "SLOW32.h"
#include "llvm/MC/TargetRegistry.h"

#define DEBUG_TYPE "slow32-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "SLOW32GenSubtargetInfo.inc"

using namespace llvm;

SLOW32Subtarget::SLOW32Subtarget(const Triple &TT, StringRef CPU, StringRef FS,
                                   const TargetMachine &TM)
    : SLOW32GenSubtargetInfo(TT, CPU, /*TuneCPU*/ CPU, FS),
      FrameLowering(), InstrInfo(*this), RegInfo(), TLInfo(TM) {
  
  // Select default CPU if none provided
  std::string CPUName = std::string(CPU);
  if (CPUName.empty())
    CPUName = "generic-slow32";
    
  // Parse features from CPU model and feature string
  ParseSubtargetFeatures(CPUName, /*TuneCPU*/ CPUName, FS);
  
  // Always enable base integer instructions
  HasStdExtI = true;
  
  // Enable multiply/divide by default for most CPUs
  if (CPUName != "slow32-minimal")
    HasStdExtM = true;
}