//===-- SLOW32AsmBackend.cpp - SLOW32 Assembler Backend ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32FixupKinds.h"
#include "MCTargetDesc/SLOW32MCTargetDesc.h"

#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <utility>

using namespace llvm;

namespace {

static uint32_t encodeBImmediate(int32_t Offset) {
  uint32_t Imm = static_cast<uint32_t>(Offset) & 0x1FFF;
  uint32_t Bit12 = ((Imm >> 12) & 0x1) << 31;
  uint32_t Bit11 = ((Imm >> 11) & 0x1) << 7;
  uint32_t Bits10_5 = ((Imm >> 5) & 0x3F) << 25;
  uint32_t Bits4_1 = ((Imm >> 1) & 0xF) << 8;
  return Bit12 | Bit11 | Bits10_5 | Bits4_1;
}

static uint32_t encodeJImmediate(int32_t Offset) {
  uint32_t Imm = static_cast<uint32_t>(Offset) & 0x1FFFFF;
  uint32_t Bit20 = ((Imm >> 20) & 0x1) << 31;
  uint32_t Bits10_1 = ((Imm >> 1) & 0x3FF) << 21;
  uint32_t Bit11 = ((Imm >> 11) & 0x1) << 20;
  uint32_t Bits19_12 = ((Imm >> 12) & 0xFF) << 12;
  return Bit20 | Bits10_1 | Bit11 | Bits19_12;
}

static uint64_t adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {
  switch (Fixup.getKind()) {
  default:
    return Value;
  case SLOW32::fixup_slow32_32:
    return Value & 0xFFFFFFFFull;
  case SLOW32::fixup_slow32_hi20:
    return ((Value + 0x800ull) >> 12) & 0xFFFFF;
  case SLOW32::fixup_slow32_lo12:
    return Value & 0xFFF;
  case SLOW32::fixup_slow32_branch: {
    int64_t Signed = static_cast<int64_t>(Value);
    if (!isInt<13>(Signed))
      Ctx.reportError(Fixup.getLoc(), "branch target out of range");
    if (Signed & 0x1)
      Ctx.reportError(Fixup.getLoc(), "branch target must be 2-byte aligned");
    return encodeBImmediate(static_cast<int32_t>(Signed));
  }
  case SLOW32::fixup_slow32_jal: {
    int64_t Signed = static_cast<int64_t>(Value);
    if (!isInt<21>(Signed))
      Ctx.reportError(Fixup.getLoc(), "jump target out of range");
    if (Signed & 0x1)
      Ctx.reportError(Fixup.getLoc(), "jump target must be 2-byte aligned");
    return encodeJImmediate(static_cast<int32_t>(Signed));
  }
  }
}

static bool isOutOfRangeBranchOffset(int64_t Offset) {
  return Offset < -4096 || Offset > 4094;
}

static bool isOutOfRangeJumpOffset(int64_t Offset) {
  return Offset < -1048576 || Offset > 1048574;
}

static unsigned getRelaxedOpcode(unsigned Opcode) {
  switch (Opcode) {
  case SLOW32::BR:
    return SLOW32::PseudoLongBR;
  case SLOW32::BEQ:
  case SLOW32::BEQ_pat:
    return SLOW32::PseudoLongBEQ;
  case SLOW32::BNE:
  case SLOW32::BNE_pat:
    return SLOW32::PseudoLongBNE;
  case SLOW32::BLT:
    return SLOW32::PseudoLongBLT;
  case SLOW32::BGE:
    return SLOW32::PseudoLongBGE;
  case SLOW32::BGT:
    return SLOW32::PseudoLongBGT;
  case SLOW32::BLE:
    return SLOW32::PseudoLongBLE;
  case SLOW32::BLTU:
    return SLOW32::PseudoLongBLTU;
  case SLOW32::BGEU:
    return SLOW32::PseudoLongBGEU;
  case SLOW32::BGTU:
    return SLOW32::PseudoLongBGTU;
  case SLOW32::BLEU:
    return SLOW32::PseudoLongBLEU;
  default:
    return Opcode;
  }
}

class SLOW32AsmBackend final : public MCAsmBackend {
  uint8_t OSABI;

public:
  explicit SLOW32AsmBackend(uint8_t OSABI)
      : MCAsmBackend(llvm::endianness::little), OSABI(OSABI) {}

  bool mayNeedRelaxation(unsigned Opcode, ArrayRef<MCOperand> Operands,
                         const MCSubtargetInfo &STI) const override {
    (void)Operands;
    (void)STI;

    switch (Opcode) {
    case SLOW32::BR:
    case SLOW32::BEQ:
    case SLOW32::BEQ_pat:
    case SLOW32::BNE:
    case SLOW32::BNE_pat:
    case SLOW32::BLT:
    case SLOW32::BGE:
    case SLOW32::BGT:
    case SLOW32::BLE:
    case SLOW32::BLTU:
    case SLOW32::BGEU:
    case SLOW32::BGTU:
    case SLOW32::BLEU:
      return true;
    default:
      return false;
    }
  }

  bool fixupNeedsRelaxation(const MCFixup &Fixup,
                            uint64_t Value) const override {
    int64_t Offset = static_cast<int64_t>(Value);
    switch (Fixup.getKind()) {
    case SLOW32::fixup_slow32_branch:
      return isOutOfRangeBranchOffset(Offset);
    case SLOW32::fixup_slow32_jal:
      return isOutOfRangeJumpOffset(Offset);
    default:
      return false;
    }
  }

  void relaxInstruction(MCInst &Inst,
                        const MCSubtargetInfo &STI) const override {
    (void)STI;
    unsigned RelaxedOp = getRelaxedOpcode(Inst.getOpcode());
    assert(RelaxedOp != Inst.getOpcode() &&
           "relaxInstruction called on non-relaxable opcode");

    MCInst Relaxed;
    Relaxed.setOpcode(RelaxedOp);

    // Use the architecturally reserved R2 as the scratch register for the
    // materialised long-branch sequence. The compiler never allocates R2, so
    // assembled sources that rely on relaxation remain correct without having
    // to plumb extra operands through the front-end syntax.
    Relaxed.addOperand(MCOperand::createReg(SLOW32::R2));

    for (unsigned I = 0, E = Inst.getNumOperands(); I < E; ++I)
      Relaxed.addOperand(Inst.getOperand(I));

    Inst = std::move(Relaxed);
  }

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override {
    return createSLOW32ELFObjectWriter(OSABI);
  }

  MCFixupKindInfo getFixupKindInfo(MCFixupKind Kind) const override {
    static const MCFixupKindInfo Infos[SLOW32::NumTargetFixupKinds] = {
        {"fixup_slow32_32", 0, 32, 0},
        {"fixup_slow32_hi20", 12, 20, 0},
        {"fixup_slow32_lo12", 20, 12, 0},
        {"fixup_slow32_branch", 0, 32, 0},
        {"fixup_slow32_jal", 0, 32, 0},
    };

    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);

    unsigned Index = Kind - FirstTargetFixupKind;
    assert(Index < (sizeof(Infos) / sizeof(Infos[0])) && "Invalid fixup kind");
    return Infos[Index];
  }

  void applyFixup(const MCFragment &F, const MCFixup &Fixup,
                  const MCValue &Target, uint8_t *Data, uint64_t Value,
                  bool IsResolved) override {
    maybeAddReloc(F, Fixup, Target, Value, IsResolved);

    if (mc::isRelocation(Fixup.getKind()))
      return;

    if (!Value)
      return;

    MCContext &Ctx = getContext();
    Value = adjustFixupValue(Fixup, Value, Ctx);

    if (!Value)
      return;

    MCFixupKindInfo Info = getFixupKindInfo(Fixup.getKind());
    Value <<= Info.TargetOffset;

    unsigned NumBytes = alignTo(Info.TargetSize + Info.TargetOffset, 8) / 8;
    assert(Fixup.getOffset() + NumBytes <= F.getSize() &&
           "Invalid fixup offset");

    for (unsigned I = 0; I != NumBytes; ++I)
      Data[I] |= uint8_t((Value >> (I * 8)) & 0xff);
  }

  bool writeNopData(raw_ostream &OS, uint64_t Count,
                    const MCSubtargetInfo *STI) const override {
    if (Count % 4 != 0)
      return false;

    for (uint64_t I = 0; I < Count; I += 4)
      support::endian::write<uint32_t>(OS, 0, Endian);

    return true;
  }
};

} // end anonymous namespace

MCAsmBackend *llvm::createSLOW32AsmBackend(const Target &T,
                                           const MCSubtargetInfo &STI,
                                           const MCRegisterInfo &MRI,
                                           const MCTargetOptions &Options) {
  (void)T;
  (void)STI;
  (void)MRI;
  (void)Options;

  return new SLOW32AsmBackend(/*OSABI=*/0);
}
