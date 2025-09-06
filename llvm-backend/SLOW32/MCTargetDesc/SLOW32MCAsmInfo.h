//===-- SLOW32MCAsmInfo.h - SLOW32 Asm Info ------------------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCASMINFO_H
#define LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class SLOW32MCAsmInfo : public MCAsmInfoELF {
public:
  explicit SLOW32MCAsmInfo(const Triple &TT);
};

} // end namespace llvm

#endif