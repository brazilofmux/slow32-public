//===--- SLOW32.cpp - Implement SLOW32 target feature support ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements SLOW32 TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "SLOW32.h"
#include "clang/Basic/MacroBuilder.h"

using namespace clang;
using namespace clang::targets;

void SLOW32TargetInfo::getTargetDefines(const LangOptions &Opts,
                                        MacroBuilder &Builder) const {
  Builder.defineMacro("__slow32__");
  Builder.defineMacro("__SLOW32__");
  Builder.defineMacro("__slow32");
  Builder.defineMacro("__SLOW32");
}

ArrayRef<const char *> SLOW32TargetInfo::getGCCRegNames() const {
  static const char *const GCCRegNames[] = {
    // 32 general purpose registers
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
    
    // Aliases for special registers
    "zero", "rv", "t0", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s0", "s1", "s2", "s3", "s4",
    "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12",
    "s13", "s14", "s15", "s16", "s17", "sp", "fp", "lr"
  };
  return llvm::ArrayRef(GCCRegNames);
}