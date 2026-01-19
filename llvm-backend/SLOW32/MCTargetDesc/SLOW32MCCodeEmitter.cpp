//===-- SLOW32MCCodeEmitter.cpp - Encode SLOW32 instructions ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32FixupKinds.h"
#include "MCTargetDesc/SLOW32MCExpr.h"
#include "MCTargetDesc/SLOW32MCTargetDesc.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"

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
                        const MCSubtargetInfo &STI) const override;

  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const {
    if (MO.isReg())
      return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
    if (MO.isImm())
      return static_cast<unsigned>(MO.getImm());
    return 0;
  }

private:
  unsigned getRegEncoding(const MCOperand &MO) const;
  uint32_t encodeRType(uint32_t Opcode, const MCInst &MI) const;
  uint32_t encodeIType(uint32_t Opcode, const MCInst &MI, unsigned RdIdx,
                       unsigned Rs1Idx, unsigned ImmIdx,
                       SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeSType(uint32_t Opcode, const MCInst &MI, unsigned Rs1Idx,
                       unsigned Rs2Idx, unsigned ImmIdx,
                       SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeBType(uint32_t Opcode, const MCInst &MI, unsigned Rs1Idx,
                       unsigned Rs2Idx, unsigned ImmIdx,
                       SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeUType(uint32_t Opcode, const MCInst &MI, unsigned RdIdx,
                       unsigned ImmIdx, SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeJType(uint32_t Opcode, unsigned Rd, const MCInst &MI,
                       unsigned TargetIdx, SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeJalr(uint32_t Opcode, const MCInst &MI,
                      SmallVectorImpl<MCFixup> &Fixups) const;
  uint32_t encodeRetLike(uint32_t Opcode, unsigned Rd, unsigned Rs1,
                         int32_t Imm) const;
  void addFixup(SmallVectorImpl<MCFixup> &Fixups, const MCExpr *Expr,
                unsigned Kind, bool IsPCRel) const;
  void expandLongUncondBr(const MCInst &MI, SmallVectorImpl<char> &CB,
                          SmallVectorImpl<MCFixup> &Fixups,
                          const MCSubtargetInfo &STI) const;
  void expandLongCondBr(const MCInst &MI, SmallVectorImpl<char> &CB,
                        SmallVectorImpl<MCFixup> &Fixups,
                        const MCSubtargetInfo &STI) const;
  void emitAbsoluteJumpSequence(unsigned ScratchReg, const MCExpr *Target,
                                SmallVectorImpl<char> &CB,
                                SmallVectorImpl<MCFixup> &Fixups,
                                const MCSubtargetInfo &STI) const;
};
} // end anonymous namespace

MCCodeEmitter *llvm::createSLOW32MCCodeEmitter(const MCInstrInfo &MCII,
                                                MCContext &Ctx) {
  return new SLOW32MCCodeEmitter(MCII, Ctx);
}

static uint32_t encodeIImmediate(int32_t Imm) {
  return (static_cast<uint32_t>(Imm) & 0xFFF) << 20;
}

static uint32_t encodeSImmediate(int32_t Imm) {
  Imm &= 0xFFF;
  uint32_t Lo = (Imm & 0x1F) << 7;
  uint32_t Hi = ((Imm >> 5) & 0x7F) << 25;
  return Lo | Hi;
}

static uint32_t encodeBImmediate(int32_t Imm) {
  Imm &= 0x1FFF;
  uint32_t Bit12 = ((Imm >> 12) & 0x1) << 31;
  uint32_t Bit11 = ((Imm >> 11) & 0x1) << 7;
  uint32_t Bits10_5 = ((Imm >> 5) & 0x3F) << 25;
  uint32_t Bits4_1 = ((Imm >> 1) & 0xF) << 8;
  return Bit12 | Bit11 | Bits10_5 | Bits4_1;
}

static uint32_t encodeJImmediate(int32_t Imm) {
  Imm &= 0x1FFFFF;
  uint32_t Bit20 = ((Imm >> 20) & 0x1) << 31;
  uint32_t Bits10_1 = ((Imm >> 1) & 0x3FF) << 21;
  uint32_t Bit11 = ((Imm >> 11) & 0x1) << 20;
  uint32_t Bits19_12 = ((Imm >> 12) & 0xFF) << 12;
  return Bit20 | Bits10_1 | Bit11 | Bits19_12;
}

void SLOW32MCCodeEmitter::addFixup(SmallVectorImpl<MCFixup> &Fixups,
                                   const MCExpr *Expr, unsigned Kind,
                                   bool IsPCRel) const {
  Fixups.push_back(
      MCFixup::create(0, Expr, static_cast<MCFixupKind>(Kind), IsPCRel));
}

static unsigned getInvertedBranchOpcode(unsigned Opcode) {
  switch (Opcode) {
  case SLOW32::PseudoLongBEQ:
    return SLOW32::BNE;
  case SLOW32::PseudoLongBNE:
    return SLOW32::BEQ;
  case SLOW32::PseudoLongBLT:
    return SLOW32::BGE;
  case SLOW32::PseudoLongBGE:
    return SLOW32::BLT;
  case SLOW32::PseudoLongBGT:
    return SLOW32::BLE;
  case SLOW32::PseudoLongBLE:
    return SLOW32::BGT;
  case SLOW32::PseudoLongBLTU:
    return SLOW32::BGEU;
  case SLOW32::PseudoLongBGEU:
    return SLOW32::BLTU;
  case SLOW32::PseudoLongBGTU:
    return SLOW32::BLEU;
  case SLOW32::PseudoLongBLEU:
    return SLOW32::BGTU;
  default:
    llvm_unreachable("Unsupported pseudo long branch opcode");
  }
}

static const MCExpr *getHiLoExpr(const MCExpr *Expr, bool Hi, MCContext &Ctx) {
  auto Kind = Hi ? SLOW32MCExpr::VK_SLOW32_HI : SLOW32MCExpr::VK_SLOW32_LO;
  return SLOW32MCExpr::create(Kind, Expr, Ctx);
}

static const MCExpr *getTargetExpr(const MCOperand &Op, MCContext &Ctx) {
  if (Op.isExpr())
    return Op.getExpr();
  assert(Op.isImm() && "Expected branch target to be expression or immediate");
  return MCConstantExpr::create(Op.getImm(), Ctx);
}

void SLOW32MCCodeEmitter::emitAbsoluteJumpSequence(
    unsigned ScratchReg, const MCExpr *Target, SmallVectorImpl<char> &CB,
    SmallVectorImpl<MCFixup> &Fixups, const MCSubtargetInfo &STI) const {
  const MCExpr *HiExpr = getHiLoExpr(Target, true, Ctx);
  const MCExpr *LoExpr = getHiLoExpr(Target, false, Ctx);

  MCInst LUI;
  LUI.setOpcode(SLOW32::LUI);
  LUI.addOperand(MCOperand::createReg(ScratchReg));
  LUI.addOperand(MCOperand::createExpr(HiExpr));
  encodeInstruction(LUI, CB, Fixups, STI);

  MCInst ADDI;
  ADDI.setOpcode(SLOW32::ADDI);
  ADDI.addOperand(MCOperand::createReg(ScratchReg));
  ADDI.addOperand(MCOperand::createReg(ScratchReg));
  ADDI.addOperand(MCOperand::createExpr(LoExpr));
  encodeInstruction(ADDI, CB, Fixups, STI);

  MCInst JALR;
  JALR.setOpcode(SLOW32::JALR);
  JALR.addOperand(MCOperand::createReg(SLOW32::R0));
  JALR.addOperand(MCOperand::createReg(ScratchReg));
  JALR.addOperand(MCOperand::createImm(0));
  encodeInstruction(JALR, CB, Fixups, STI);
}

void SLOW32MCCodeEmitter::expandLongUncondBr(
    const MCInst &MI, SmallVectorImpl<char> &CB,
    SmallVectorImpl<MCFixup> &Fixups, const MCSubtargetInfo &STI) const {
  assert(MI.getNumOperands() == 2 &&
         "Unexpected operand count for long BR");
  unsigned ScratchReg = MI.getOperand(0).getReg();
  const MCExpr *TargetExpr = getTargetExpr(MI.getOperand(1), Ctx);
  emitAbsoluteJumpSequence(ScratchReg, TargetExpr, CB, Fixups, STI);
}

void SLOW32MCCodeEmitter::expandLongCondBr(
    const MCInst &MI, SmallVectorImpl<char> &CB,
    SmallVectorImpl<MCFixup> &Fixups, const MCSubtargetInfo &STI) const {
  assert(MI.getNumOperands() == 4 &&
         "Unexpected operand count for long conditional branch");

  unsigned InvertedOpc = getInvertedBranchOpcode(MI.getOpcode());

  MCInst Skip;
  Skip.setOpcode(InvertedOpc);
  Skip.addOperand(MI.getOperand(1));
  Skip.addOperand(MI.getOperand(2));
  Skip.addOperand(MCOperand::createImm(12));
  encodeInstruction(Skip, CB, Fixups, STI);

  unsigned ScratchReg = MI.getOperand(0).getReg();
  const MCExpr *TargetExpr = getTargetExpr(MI.getOperand(3), Ctx);
  emitAbsoluteJumpSequence(ScratchReg, TargetExpr, CB, Fixups, STI);
}

unsigned SLOW32MCCodeEmitter::getRegEncoding(const MCOperand &MO) const {
  return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
}

uint32_t SLOW32MCCodeEmitter::encodeRType(uint32_t Opcode,
                                          const MCInst &MI) const {
  unsigned Rd = getRegEncoding(MI.getOperand(0));
  unsigned Rs1 = getRegEncoding(MI.getOperand(1));
  unsigned Rs2 = getRegEncoding(MI.getOperand(2));
  return Opcode | (Rd << 7) | (Rs1 << 15) | (Rs2 << 20);
}

uint32_t SLOW32MCCodeEmitter::encodeIType(uint32_t Opcode, const MCInst &MI,
                                          unsigned RdIdx, unsigned Rs1Idx,
                                          unsigned ImmIdx,
                                          SmallVectorImpl<MCFixup> &Fixups) const {
  unsigned Rd = getRegEncoding(MI.getOperand(RdIdx));
  unsigned Rs1 = getRegEncoding(MI.getOperand(Rs1Idx));
  const MCOperand &ImmOp = MI.getOperand(ImmIdx);

  uint32_t ImmBits = 0;
  if (ImmOp.isImm()) {
    int32_t Imm = static_cast<int32_t>(ImmOp.getImm());
    ImmBits = encodeIImmediate(Imm);
  } else if (ImmOp.isExpr()) {
    const MCExpr *Expr = ImmOp.getExpr();
    if (const auto *SlowExpr = dyn_cast<SLOW32MCExpr>(Expr)) {
      switch (SlowExpr->getKind()) {
      case SLOW32MCExpr::VK_SLOW32_HI:
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_hi20, false);
        break;
      case SLOW32MCExpr::VK_SLOW32_LO:
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_lo12, false);
        break;
      case SLOW32MCExpr::VK_SLOW32_None:
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_lo12, false);
        break;
      }
    } else {
      addFixup(Fixups, Expr, SLOW32::fixup_slow32_lo12, false);
    }
  }

  return Opcode | (Rd << 7) | (Rs1 << 15) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeSType(uint32_t Opcode, const MCInst &MI,
                                          unsigned Rs1Idx, unsigned Rs2Idx,
                                          unsigned ImmIdx,
                                          SmallVectorImpl<MCFixup> &Fixups) const {
  unsigned Rs1 = getRegEncoding(MI.getOperand(Rs1Idx));
  unsigned Rs2 = getRegEncoding(MI.getOperand(Rs2Idx));
  const MCOperand &ImmOp = MI.getOperand(ImmIdx);

  uint32_t ImmBits = 0;
  if (ImmOp.isImm()) {
    int32_t Imm = static_cast<int32_t>(ImmOp.getImm());
    ImmBits = encodeSImmediate(Imm);
  } else if (ImmOp.isExpr()) {
    const MCExpr *Expr = ImmOp.getExpr();
    if (const auto *SlowExpr = dyn_cast<SLOW32MCExpr>(Expr)) {
      if (SlowExpr->getKind() == SLOW32MCExpr::VK_SLOW32_LO)
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_lo12, false);
      else
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_lo12, false);
    } else {
      addFixup(Fixups, Expr, SLOW32::fixup_slow32_lo12, false);
    }
  }

  return Opcode | (Rs1 << 15) | (Rs2 << 20) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeBType(uint32_t Opcode, const MCInst &MI,
                                          unsigned Rs1Idx, unsigned Rs2Idx,
                                          unsigned ImmIdx,
                                          SmallVectorImpl<MCFixup> &Fixups) const {
  unsigned Rs1 = getRegEncoding(MI.getOperand(Rs1Idx));
  unsigned Rs2 = getRegEncoding(MI.getOperand(Rs2Idx));
  const MCOperand &ImmOp = MI.getOperand(ImmIdx);

  uint32_t ImmBits = 0;
  if (ImmOp.isImm()) {
    int32_t Imm = static_cast<int32_t>(ImmOp.getImm());
    ImmBits = encodeBImmediate(Imm);
  } else if (ImmOp.isExpr()) {
    addFixup(Fixups, ImmOp.getExpr(), SLOW32::fixup_slow32_branch, true);
  }

  return Opcode | (Rs1 << 15) | (Rs2 << 20) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeUType(uint32_t Opcode, const MCInst &MI,
                                          unsigned RdIdx, unsigned ImmIdx,
                                          SmallVectorImpl<MCFixup> &Fixups) const {
  unsigned Rd = getRegEncoding(MI.getOperand(RdIdx));
  const MCOperand &ImmOp = MI.getOperand(ImmIdx);

  uint32_t ImmBits = 0;
  if (ImmOp.isImm()) {
    int32_t Imm = static_cast<int32_t>(ImmOp.getImm());
    ImmBits = (static_cast<uint32_t>(Imm) & 0xFFFFF) << 12;
  } else if (ImmOp.isExpr()) {
    const MCExpr *Expr = ImmOp.getExpr();
    if (const auto *SlowExpr = dyn_cast<SLOW32MCExpr>(Expr)) {
      if (SlowExpr->getKind() == SLOW32MCExpr::VK_SLOW32_HI)
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_hi20, false);
      else
        addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_hi20, false);
    } else {
      addFixup(Fixups, Expr, SLOW32::fixup_slow32_hi20, false);
    }
  }

  return Opcode | (Rd << 7) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeJType(uint32_t Opcode, unsigned Rd,
                                          const MCInst &MI, unsigned TargetIdx,
                                          SmallVectorImpl<MCFixup> &Fixups) const {
  const MCOperand &Target = MI.getOperand(TargetIdx);
  uint32_t ImmBits = 0;

  if (Target.isImm()) {
    int32_t Imm = static_cast<int32_t>(Target.getImm());
    ImmBits = encodeJImmediate(Imm);
  } else if (Target.isExpr()) {
    addFixup(Fixups, Target.getExpr(), SLOW32::fixup_slow32_jal, true);
  }

  return Opcode | (Rd << 7) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeJalr(
    uint32_t Opcode, const MCInst &MI,
    SmallVectorImpl<MCFixup> &Fixups) const {
  assert(MI.getNumOperands() >= 2 && "JALR expects at least rd, rs1");
  unsigned Rd = getRegEncoding(MI.getOperand(0));
  unsigned Rs1 = getRegEncoding(MI.getOperand(1));

  uint32_t ImmBits = encodeIImmediate(0);
  for (unsigned I = 2, E = MI.getNumOperands(); I < E; ++I) {
    const MCOperand &Op = MI.getOperand(I);
    if (Op.isImm()) {
      ImmBits = encodeIImmediate(static_cast<int32_t>(Op.getImm()));
      break;
    }
    if (Op.isExpr()) {
      const MCExpr *Expr = Op.getExpr();
      if (const auto *SlowExpr = dyn_cast<SLOW32MCExpr>(Expr)) {
        switch (SlowExpr->getKind()) {
        case SLOW32MCExpr::VK_SLOW32_HI:
          addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_hi20, false);
          break;
        case SLOW32MCExpr::VK_SLOW32_LO:
        case SLOW32MCExpr::VK_SLOW32_None:
          addFixup(Fixups, SlowExpr, SLOW32::fixup_slow32_lo12, false);
          break;
        }
      } else {
        addFixup(Fixups, Expr, SLOW32::fixup_slow32_lo12, false);
      }
      ImmBits = 0;
      break;
    }
  }

  return Opcode | (Rd << 7) | (Rs1 << 15) | ImmBits;
}

uint32_t SLOW32MCCodeEmitter::encodeRetLike(uint32_t Opcode, unsigned Rd,
                                            unsigned Rs1, int32_t Imm) const {
  return Opcode | (Rd << 7) | (Rs1 << 15) | encodeIImmediate(Imm);
}

void SLOW32MCCodeEmitter::encodeInstruction(const MCInst &MI,
                                            SmallVectorImpl<char> &CB,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  uint32_t Binary = 0;
  unsigned Opc = MI.getOpcode();

  switch (Opc) {
  default:
    llvm_unreachable("Unhandled SLOW32 opcode");

  case SLOW32::PseudoLongBR:
    expandLongUncondBr(MI, CB, Fixups, STI);
    return;
  case SLOW32::PseudoLongBEQ:
  case SLOW32::PseudoLongBNE:
  case SLOW32::PseudoLongBLT:
  case SLOW32::PseudoLongBGE:
  case SLOW32::PseudoLongBGT:
  case SLOW32::PseudoLongBLE:
  case SLOW32::PseudoLongBLTU:
  case SLOW32::PseudoLongBGEU:
  case SLOW32::PseudoLongBGTU:
  case SLOW32::PseudoLongBLEU:
    expandLongCondBr(MI, CB, Fixups, STI);
    return;

  case SLOW32::ADD:
    Binary = encodeRType(0x00, MI);
    break;
  case SLOW32::SUB:
    Binary = encodeRType(0x01, MI);
    break;
  case SLOW32::XOR:
    Binary = encodeRType(0x02, MI);
    break;
  case SLOW32::OR:
    Binary = encodeRType(0x03, MI);
    break;
  case SLOW32::AND:
    Binary = encodeRType(0x04, MI);
    break;
  case SLOW32::SLL:
    Binary = encodeRType(0x05, MI);
    break;
  case SLOW32::SRL:
    Binary = encodeRType(0x06, MI);
    break;
  case SLOW32::SRA:
    Binary = encodeRType(0x07, MI);
    break;
  case SLOW32::SLT:
    Binary = encodeRType(0x08, MI);
    break;
  case SLOW32::SLTU:
    Binary = encodeRType(0x09, MI);
    break;
  case SLOW32::MUL:
    Binary = encodeRType(0x0A, MI);
    break;
  case SLOW32::DIV:
    Binary = encodeRType(0x0C, MI);
    break;
  case SLOW32::REM:
    Binary = encodeRType(0x0D, MI);
    break;
  case SLOW32::SEQ:
    Binary = encodeRType(0x0E, MI);
    break;
  case SLOW32::SNE:
    Binary = encodeRType(0x0F, MI);
    break;
  case SLOW32::SGT:
    Binary = encodeRType(0x18, MI);
    break;
  case SLOW32::SGTU:
    Binary = encodeRType(0x19, MI);
    break;
  case SLOW32::SLE:
    Binary = encodeRType(0x1A, MI);
    break;
  case SLOW32::SLEU:
    Binary = encodeRType(0x1B, MI);
    break;
  case SLOW32::SGE:
    Binary = encodeRType(0x1C, MI);
    break;
  case SLOW32::SGEU:
    Binary = encodeRType(0x1D, MI);
    break;

  case SLOW32::ADDI:
    Binary = encodeIType(0x10, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::ORI:
    Binary = encodeIType(0x11, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::ANDI:
    Binary = encodeIType(0x12, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::SLLI:
    Binary = encodeIType(0x13, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::SRLI:
    Binary = encodeIType(0x14, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::SRAI:
    Binary = encodeIType(0x15, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::XORI:
    Binary = encodeIType(0x1E, MI, 0, 1, 2, Fixups);
    break;

  case SLOW32::LUI:
    Binary = encodeUType(0x20, MI, 0, 1, Fixups);
    break;

  case SLOW32::LDB:
    Binary = encodeIType(0x30, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::LDH:
    Binary = encodeIType(0x31, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::LDW:
    Binary = encodeIType(0x32, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::LDBU:
    Binary = encodeIType(0x33, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::LDHU:
    Binary = encodeIType(0x34, MI, 0, 1, 2, Fixups);
    break;

  case SLOW32::STB:
    Binary = encodeSType(0x38, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::STH:
    Binary = encodeSType(0x39, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::STW:
    Binary = encodeSType(0x3A, MI, 0, 1, 2, Fixups);
    break;

  case SLOW32::JAL:
  case SLOW32::JAL_CALLR:
    Binary = encodeJType(0x40, 31, MI, 0, Fixups);
    break;
  case SLOW32::BR:
    Binary = encodeJType(0x40, 0, MI, 0, Fixups);
    break;

  case SLOW32::JALR:
  case SLOW32::JALR_CALLR:
    Binary = encodeJalr(0x41, MI, Fixups);
    break;
  case SLOW32::RET:
    Binary = encodeRetLike(0x41, 0, 31, 0);
    break;
  case SLOW32::RET64:
    Binary = encodeRetLike(0x41, 0, 31, 0);
    break;

  case SLOW32::BEQ_pat:
  case SLOW32::BEQ:
    Binary = encodeBType(0x48, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BNE_pat:
  case SLOW32::BNE:
    Binary = encodeBType(0x49, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BLT:
    Binary = encodeBType(0x4A, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BGE:
    Binary = encodeBType(0x4B, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BLTU:
    Binary = encodeBType(0x4C, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BGEU:
    Binary = encodeBType(0x4D, MI, 0, 1, 2, Fixups);
    break;
  case SLOW32::BGT:
    Binary = encodeBType(0x4A, MI, 1, 0, 2, Fixups);
    break;
  case SLOW32::BLE:
    Binary = encodeBType(0x4B, MI, 1, 0, 2, Fixups);
    break;
  case SLOW32::BGTU:
    Binary = encodeBType(0x4C, MI, 1, 0, 2, Fixups);
    break;
  case SLOW32::BLEU:
    Binary = encodeBType(0x4D, MI, 1, 0, 2, Fixups);
    break;
  }

  support::endian::write<uint32_t>(CB, Binary, llvm::endianness::little);
}
