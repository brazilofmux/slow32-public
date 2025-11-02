//===-- SLOW32FixupKinds.h - SLOW32 Specific Fixup Entries -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32FIXUPKINDS_H
#define LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32FIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm::SLOW32 {

enum Fixups {
  fixup_slow32_32 = FirstTargetFixupKind,
  fixup_slow32_hi20,
  fixup_slow32_lo12,
  fixup_slow32_branch,
  fixup_slow32_jal,

  fixup_slow32_invalid,
  NumTargetFixupKinds = fixup_slow32_invalid - FirstTargetFixupKind
};

} // end namespace llvm::SLOW32

#endif
