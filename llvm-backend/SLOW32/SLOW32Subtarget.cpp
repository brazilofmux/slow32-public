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

void SLOW32Subtarget::initLibcallLoweringInfo(LibcallLoweringInfo &Info) const {
  // SLOW32 is not recognized in LLVM's RuntimeLibcalls infrastructure,
  // so we need to explicitly set which libcall implementations to use.
  // These map to the compiler-rt/libgcc style helpers that our crt ships.

  // i64 division/remainder
  Info.setLibcallImpl(RTLIB::SDIV_I64, RTLIB::impl___divdi3);
  Info.setLibcallImpl(RTLIB::UDIV_I64, RTLIB::impl___udivdi3);
  Info.setLibcallImpl(RTLIB::SREM_I64, RTLIB::impl___moddi3);
  Info.setLibcallImpl(RTLIB::UREM_I64, RTLIB::impl___umoddi3);

  // i32 unsigned division/remainder (SLOW32 only has signed DIV/REM instructions)
  Info.setLibcallImpl(RTLIB::UDIV_I32, RTLIB::impl___udivsi3);
  Info.setLibcallImpl(RTLIB::UREM_I32, RTLIB::impl___umodsi3);

  // The runtime ships native C implementations of the basic memory helpers.
  Info.setLibcallImpl(RTLIB::MEMCPY, RTLIB::impl_memcpy);
  Info.setLibcallImpl(RTLIB::MEMMOVE, RTLIB::impl_memmove);
  Info.setLibcallImpl(RTLIB::MEMSET, RTLIB::impl_memset);
}