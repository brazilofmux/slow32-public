//===-- SLOW32InstPrinter.cpp - SLOW32 MCInst Printer -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32InstPrinter.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-instprinter"

// Include the auto-generated portion
#include "SLOW32GenAsmWriter.inc"

void SLOW32InstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                   StringRef Annot, const MCSubtargetInfo &STI,
                                   raw_ostream &O) {
  printInstruction(MI, Address, STI, O);
  printAnnotation(O, Annot);
}

void SLOW32InstPrinter::printRegName(raw_ostream &O, MCRegister Reg) {
  O << getRegisterName(Reg);
}

void SLOW32InstPrinter::printOperand(const MCInst *MI, unsigned OpNum,
                                      const MCSubtargetInfo &STI, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNum);

  if (Op.isReg()) {
    printRegName(O, Op.getReg());
  } else if (Op.isImm()) {
    // Check if this is an ADDI instruction that needs sign-extended 12-bit
    // immediate. MCInstrInfo returns StringRef, so use it directly rather than
    // assuming a null-terminated C string.
    StringRef Mnemonic = MII.getName(MI->getOpcode());

    // Special handling for ADDI instruction's immediate operand
    // ADDI uses 12-bit signed immediates that need sign extension when printing
    if (Mnemonic == "ADDI" && OpNum == 2) {
      int64_t Imm = Op.getImm();
      // Mask to 12 bits first in case larger value is stored
      Imm = Imm & 0xFFF;
      // If bit 11 is set, sign-extend from 12 bits
      if (Imm & 0x800) {
        Imm = Imm - 0x1000;
      }
      O << Imm;
    } else {
      O << Op.getImm();
    }
  } else if (Op.isExpr()) {
    // For SLOW32, we need to handle %hi/%lo fixups when the operand is a
    // symbolic expression. We drive this off the opcode until we plumb
    // dedicated MCExprs through the MC layer.
    StringRef Mnemonic = MII.getName(MI->getOpcode());

    // Check if this is a LUI instruction (needs %hi)
    if (Mnemonic == "LUI") {
      O << "%hi(";
      MAI.printExpr(O, *Op.getExpr());
      O << ")";
    }
    // ADDI materialises the low 12 bits of an address.
    else if (Mnemonic == "ADDI") {
      O << "%lo(";
      MAI.printExpr(O, *Op.getExpr());
      O << ")";
    }
    // Keep supporting legacy ORI-based materialisation for now.
    else if (Mnemonic == "ORI") {
      O << "%lo(";
      MAI.printExpr(O, *Op.getExpr());
      O << ")";
    }
    else {
      // For other instructions, print the expression normally
      MAI.printExpr(O, *Op.getExpr());
    }
  } else {
    llvm_unreachable("Unknown operand type");
  }
}

void SLOW32InstPrinter::printBranchTarget(const MCInst *MI, unsigned OpNum,
                                          const MCSubtargetInfo &STI, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNum);
  
  if (Op.isImm()) {
    // For immediate branch targets, this shouldn't happen in normal flow
    // but print something readable
    O << ".LBB_" << Op.getImm();
  } else if (Op.isExpr()) {
    // For symbolic expressions (the normal case for labels)
    const MCExpr *Expr = Op.getExpr();
    if (auto *SymRef = dyn_cast<MCSymbolRefExpr>(Expr)) {
      // Print the symbol name directly
      O << SymRef->getSymbol().getName();
    } else {
      // For other expression types, print the address (shouldn't happen normally)
      O << Expr;
    }
  } else {
    // Fallback to regular operand printing
    printOperand(MI, OpNum, STI, O);
  }
}
