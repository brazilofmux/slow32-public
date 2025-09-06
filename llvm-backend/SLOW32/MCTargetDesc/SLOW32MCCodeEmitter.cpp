//===-- SLOW32MCCodeEmitter.cpp - Encode SLOW32 instructions ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32MCTargetDesc.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/EndianStream.h"

using namespace llvm;

namespace {
class SLOW32MCCodeEmitter : public MCCodeEmitter {
  const MCInstrInfo &MCII;
  MCContext &Ctx;

public:
  SLOW32MCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx)
      : MCII(MCII), Ctx(Ctx) {}

  ~SLOW32MCCodeEmitter() override = default;

  void encodeInstruction(const MCInst &MI, SmallVectorImpl<char> &CB,
                        SmallVectorImpl<MCFixup> &Fixups,
                        const MCSubtargetInfo &STI) const override {
    // For now, emit a placeholder 32-bit instruction
    support::endian::write<uint32_t>(CB, 0, llvm::endianness::little);
  }

  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const {
    if (MO.isReg())
      return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
    if (MO.isImm())
      return static_cast<unsigned>(MO.getImm());
    return 0;
  }
};
} // end anonymous namespace

MCCodeEmitter *llvm::createSLOW32MCCodeEmitter(const MCInstrInfo &MCII,
                                                MCContext &Ctx) {
  return new SLOW32MCCodeEmitter(MCII, Ctx);
}