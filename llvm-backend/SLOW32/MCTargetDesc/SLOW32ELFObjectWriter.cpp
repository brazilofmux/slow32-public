//===-- SLOW32ELFObjectWriter.cpp - SLOW32 ELF Writer -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32FixupKinds.h"
#include "MCTargetDesc/SLOW32MCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {

// SLOW-32 relocation type numbers, matching the project's object format
// (docs/file-formats.md). The machine type is 0x32, also per that document.
enum {
  R_SLOW32_NONE = 0,
  R_SLOW32_32 = 1,
  R_SLOW32_HI20 = 2,
  R_SLOW32_LO12 = 3,
  R_SLOW32_BRANCH = 4,
  R_SLOW32_JAL = 5,
};

class SLOW32ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  SLOW32ELFObjectWriter(uint8_t OSABI)
      : MCELFObjectTargetWriter(/*Is64Bit*/ false, OSABI,
                                /*EM=*/0x32, /*HasRelocationAddend*/ true) {}

  unsigned getRelocType(const MCFixup &Fixup, const MCValue &Target,
                       bool IsPCRel) const override {
    switch (Fixup.getKind()) {
    case FK_Data_4:
    case SLOW32::fixup_slow32_32:
      return R_SLOW32_32;
    case SLOW32::fixup_slow32_hi20:
      return R_SLOW32_HI20;
    case SLOW32::fixup_slow32_lo12:
      return R_SLOW32_LO12;
    case SLOW32::fixup_slow32_branch:
      return R_SLOW32_BRANCH;
    case SLOW32::fixup_slow32_jal:
      return R_SLOW32_JAL;
    default:
      reportError(Fixup.getLoc(), "unsupported relocation type");
      return R_SLOW32_NONE;
    }
  }
};
} // end anonymous namespace

std::unique_ptr<MCObjectTargetWriter>
llvm::createSLOW32ELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<SLOW32ELFObjectWriter>(OSABI);
}