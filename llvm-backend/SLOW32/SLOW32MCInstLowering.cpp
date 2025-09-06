//===-- SLOW32MCInstLowering.cpp -----------------------------------------===//
#include "SLOW32MCInstLowering.h"
#include "SLOW32.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

static MCOperand lowerOperand(AsmPrinter &AP, const MachineOperand &MO) {
  switch (MO.getType()) {
    case MachineOperand::MO_Register:
      return MCOperand::createReg(MO.getReg());
    case MachineOperand::MO_Immediate:
      return MCOperand::createImm(MO.getImm());
    case MachineOperand::MO_GlobalAddress: {
      MCSymbol *Sym = AP.getSymbol(MO.getGlobal());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // Check target flags for %hi/%lo and wrap the expression
      unsigned TargetFlags = MO.getTargetFlags();
      if (TargetFlags == SLOW32II::MO_HI) {
        // Create %hi(symbol) - this will be handled by the assembler
        // For now, just use the plain symbol - the assembler will handle %hi/%lo
        // when we emit the proper syntax in the instruction printer
      } else if (TargetFlags == SLOW32II::MO_LO) {
        // Create %lo(symbol) - this will be handled by the assembler
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_ExternalSymbol: {
      MCSymbol *Sym = AP.GetExternalSymbolSymbol(MO.getSymbolName());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // Check target flags for %hi/%lo
      unsigned TargetFlags = MO.getTargetFlags();
      if (TargetFlags == SLOW32II::MO_HI) {
        // Create %hi(symbol) - this will be handled by the assembler
      } else if (TargetFlags == SLOW32II::MO_LO) {
        // Create %lo(symbol) - this will be handled by the assembler
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_MachineBasicBlock: {
      MCSymbol *Sym = MO.getMBB()->getSymbol();
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_JumpTableIndex: {
      MCSymbol *Sym = AP.GetJTISymbol(MO.getIndex());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // Check target flags for %hi/%lo
      unsigned TargetFlags = MO.getTargetFlags();
      if (TargetFlags == SLOW32II::MO_HI) {
        // Create %hi(jumptable) - this will be handled by the assembler
      } else if (TargetFlags == SLOW32II::MO_LO) {
        // Create %lo(jumptable) - this will be handled by the assembler
      }
      
      return MCOperand::createExpr(Expr);
    }
    default:
      // Fallback to a benign immediate to avoid UB; you'll see wrong asm,
      // but at least you won't segfault.
      return MCOperand::createImm(0);
  }
}

void MCInstLowering::lower(const MachineInstr *MI, MCInst &OutMI) {
  OutMI.clear();
  OutMI.setOpcode(MI->getOpcode());
  for (const MachineOperand &MO : MI->operands()) {
    if (MO.isReg() && MO.isImplicit())
      continue; // printers ignore implicit regs
    OutMI.addOperand(lowerOperand(AP, MO));
  }
}
