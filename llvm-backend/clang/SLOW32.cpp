//===--- SLOW32.cpp - Implement SLOW32 target feature support -------------===//
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

const char *const SLOW32TargetInfo::GCCRegNames[] = {
    // 32 general purpose registers
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31"
};

ArrayRef<const char *> SLOW32TargetInfo::getGCCRegNames() const {
  return llvm::ArrayRef(GCCRegNames);
}

void SLOW32TargetInfo::getTargetDefines(const LangOptions &Opts,
                                        MacroBuilder &Builder) const {
  Builder.defineMacro("__SLOW32__");
  Builder.defineMacro("__slow32__");
  // C11's optional parts this implementation (compiler and runtime/) does
  // not have (6.10.8.3): <threads.h>.  Atomics it has: <stdatomic.h>
  // works, one thread being trivially lock-free.  Complex arithmetic it has
  // (runtime/complex_rt.c, musl's <complex.h> functions in libc).
  if (!Opts.CPlusPlus)
    Builder.defineMacro("__STDC_NO_THREADS__");
}