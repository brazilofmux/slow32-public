//===-- SLOW32MCExpr.h - SLOW32 specific MC expression classes -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCEXPR_H
#define LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCEXPR_H

#include "llvm/MC/MCExpr.h"

namespace llvm {

class MCAssembler;
class MCAsmLayout;
class MCAsmInfo;

class SLOW32MCExpr : public MCTargetExpr {
public:
  enum VariantKind { VK_SLOW32_None, VK_SLOW32_HI, VK_SLOW32_LO };

  static const SLOW32MCExpr *create(VariantKind Kind, const MCExpr *Expr,
                                    MCContext &Ctx);

  VariantKind getKind() const { return Kind; }
  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;

  bool evaluateAsRelocatableImpl(MCValue &Result, const MCAssembler *Asm) const override;

  void visitUsedExpr(MCStreamer &Streamer) const override;

  MCFragment *findAssociatedFragment() const override {
    return Expr->findAssociatedFragment();
  }

private:
  SLOW32MCExpr(VariantKind Kind, const MCExpr *Expr) : Kind(Kind), Expr(Expr) {}

  VariantKind Kind;
  const MCExpr *Expr;
};

} // end namespace llvm

#endif
