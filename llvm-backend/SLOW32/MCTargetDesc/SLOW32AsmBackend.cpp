//===-- SLOW32AsmBackend.cpp - SLOW32 Assembler Backend -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32MCTargetDesc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
// MCFixupKindInfo is part of MCAsmBackend.h
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class SLOW32AsmBackend : public MCAsmBackend {
  uint8_t OSABI;

public:
  SLOW32AsmBackend(const MCSubtargetInfo &STI, uint8_t OSABI)
      : MCAsmBackend(llvm::endianness::little), OSABI(OSABI) {}

  // Remove getNumFixupKinds - not needed anymore

  MCFixupKindInfo getFixupKindInfo(MCFixupKind Kind) const override {
    static const MCFixupKindInfo Infos[] = {
        // name, offset, bits, flags
        {"fixup_slow32_32", 0, 32, 0},
    };
    
    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);
    
    return Infos[Kind - FirstTargetFixupKind];
  }

  void applyFixup(const MCFragment &Frag, const MCFixup &Fixup,
                  const MCValue &Target, uint8_t *Data,
                  uint64_t Value, bool IsResolved) override {
    // For now, do nothing
  }

  bool mayNeedRelaxation(unsigned Opcode, ArrayRef<MCOperand> Operands,
                         const MCSubtargetInfo &STI) const override {
    return false;
  }

  bool fixupNeedsRelaxation(const MCFixup &Fixup,
                           uint64_t Value) const override {
    return false;
  }

  void relaxInstruction(MCInst &Inst,
                       const MCSubtargetInfo &STI) const override {
    llvm_unreachable("relaxInstruction not implemented");
  }

  bool writeNopData(raw_ostream &OS, uint64_t Count,
                   const MCSubtargetInfo *STI) const override {
    if ((Count % 4) != 0)
      return false;
    
    for (uint64_t i = 0; i < Count; i += 4)
      OS.write("\0\0\0\0", 4);
    
    return true;
  }

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override {
    return createSLOW32ELFObjectWriter(OSABI);
  }
};
} // end anonymous namespace

MCAsmBackend *llvm::createSLOW32AsmBackend(const Target &T,
                                            const MCSubtargetInfo &STI,
                                            const MCRegisterInfo &MRI,
                                            const MCTargetOptions &Options) {
  return new SLOW32AsmBackend(STI, /*OSABI=*/0);
}