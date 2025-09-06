//===-- SLOW32TargetInfo.cpp - SLOW32 Target Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target &getTheSLOW32Target();
}

Target &llvm::getTheSLOW32Target() {
  static Target TheSLOW32Target;
  return TheSLOW32Target;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSLOW32TargetInfo() {
  RegisterTarget<Triple::slow32> X(getTheSLOW32Target(), "slow32",
                                    "SLOW32", "SLOW32");
}