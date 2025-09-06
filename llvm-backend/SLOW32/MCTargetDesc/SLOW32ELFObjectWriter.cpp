//===-- SLOW32ELFObjectWriter.cpp - SLOW32 ELF Writer -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32MCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class SLOW32ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  SLOW32ELFObjectWriter(uint8_t OSABI)
      : MCELFObjectTargetWriter(/*Is64Bit*/ false, OSABI, 
                                /*ELF::EM_NONE*/ 0, /*HasRelocationAddend*/ true) {}

  unsigned getRelocType(const MCFixup &Fixup, const MCValue &Target,
                       bool IsPCRel) const override {
    // Return a placeholder relocation type
    return 0;
  }
};
} // end anonymous namespace

std::unique_ptr<MCObjectTargetWriter>
llvm::createSLOW32ELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<SLOW32ELFObjectWriter>(OSABI);
}