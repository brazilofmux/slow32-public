//===-- SLOW32InstPrinter.cpp - SLOW32 MCInst Printer -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32InstPrinter.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <cstring>

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
    O << Op.getImm();
  } else if (Op.isExpr()) {
    // For SLOW32, we need to handle %hi and %lo for global addresses
    // We check the instruction mnemonic to determine if this needs %hi or %lo
    // This is a simplified approach - ideally we'd use target flags from MCInstLowering
    
    // Get the instruction name from the generated instruction info
    const MCInstrDesc &Desc = MII.get(MI->getOpcode());
    const char *Mnemonic = MII.getName(MI->getOpcode()).data();
    
    // Check if this is a LUI instruction (needs %hi)
    if (Mnemonic && strstr(Mnemonic, "LUI")) {
      O << "%hi(";
      MAI.printExpr(O, *Op.getExpr());
      O << ")";
    }
    // Check if this is an ORI, ADDI or LI instruction following a pattern (needs %lo)
    else if (Mnemonic && (strstr(Mnemonic, "ORI") || strstr(Mnemonic, "ADDI") || strstr(Mnemonic, "LI"))) {
      // Check if this is for the lower part of an address
      // by looking at the register operand (should be r0 for %lo)
      if (OpNum > 1 && MI->getOperand(1).isReg()) {
        MCRegister Reg = MI->getOperand(1).getReg();
        const char *RegName = getRegisterName(Reg);
        if (RegName && strcmp(RegName, "r0") == 0) {
          O << "%lo(";
          MAI.printExpr(O, *Op.getExpr());
          O << ")";
        } else {
          // Regular expression without %lo
          MAI.printExpr(O, *Op.getExpr());
        }
      } else {
        // Regular expression
        MAI.printExpr(O, *Op.getExpr());
      }
    } else {
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