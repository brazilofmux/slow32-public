//===-- SLOW32MCExpr.cpp - SLOW32 specific MC expression classes ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SLOW32MCExpr.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

const SLOW32MCExpr *SLOW32MCExpr::create(VariantKind Kind, const MCExpr *Expr,
                                         MCContext &Ctx) {
  return new (Ctx) SLOW32MCExpr(Kind, Expr);
}

void SLOW32MCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  switch (Kind) {
  case VK_SLOW32_None:
    MAI->printExpr(OS, *Expr);
    return;
  case VK_SLOW32_HI:
    OS << "%hi(";
    break;
  case VK_SLOW32_LO:
    OS << "%lo(";
    break;
  }
  MAI->printExpr(OS, *Expr);
  OS << ')';
}

bool SLOW32MCExpr::evaluateAsRelocatableImpl(MCValue &Result,
                                             const MCAssembler *Asm) const {
  return Expr->evaluateAsRelocatable(Result, Asm);
}

void SLOW32MCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*Expr);
}
