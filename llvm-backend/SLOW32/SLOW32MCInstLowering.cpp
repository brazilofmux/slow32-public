//===-- SLOW32MCInstLowering.cpp -----------------------------------------===//
#include "SLOW32MCInstLowering.h"
#include "SLOW32.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "slow32-mcinst-lower"

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
      
      // Handle offset if present (e.g., for symbol+offset addressing)
      // GlobalAddress supports getOffset()
      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        // Create symbol+offset expression
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }
      
      // Check target flags for %hi/%lo and wrap the expression
      unsigned TargetFlags = MO.getTargetFlags();
      if (TargetFlags == SLOW32II::MO_HI) {
        // Create %hi(symbol+offset) - this will be handled by the assembler
        // For now, just use the plain symbol - the assembler will handle %hi/%lo
        // when we emit the proper syntax in the instruction printer
      } else if (TargetFlags == SLOW32II::MO_LO) {
        // Create %lo(symbol+offset) - this will be handled by the assembler
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_ExternalSymbol: {
      MCSymbol *Sym = AP.GetExternalSymbolSymbol(MO.getSymbolName());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // ExternalSymbol doesn't have offset support - only GlobalAddress and JumpTableIndex do
      
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
      
      // JumpTableIndex doesn't support getOffset() according to LLVM API
      // Jump tables don't have offsets - they're indexed by the jump table index
      
      // Check target flags for %hi/%lo
      unsigned TargetFlags = MO.getTargetFlags();
      if (TargetFlags == SLOW32II::MO_HI) {
        // Create %hi(jumptable) - this will be handled by the assembler
      } else if (TargetFlags == SLOW32II::MO_LO) {
        // Create %lo(jumptable) - this will be handled by the assembler
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_ConstantPoolIndex: {
      // Handle constant pool references
      MCSymbol *Sym = AP.GetCPISymbol(MO.getIndex());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // ConstantPoolIndex supports getOffset()
      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_BlockAddress: {
      // Handle block addresses
      MCSymbol *Sym = AP.GetBlockAddressSymbol(MO.getBlockAddress());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      
      // BlockAddress supports getOffset()
      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }
      
      return MCOperand::createExpr(Expr);
    }
    case MachineOperand::MO_Metadata:
      // Skip metadata operands - they shouldn't be lowered to MC
      return MCOperand::createImm(0);
    case MachineOperand::MO_CFIIndex:
      // Skip CFI operands - they are handled separately
      return MCOperand::createImm(0);
    default:
      // Log unknown operand type for debugging
      LLVM_DEBUG(dbgs() << "Warning: Unhandled MachineOperand type: " << MO.getType() << "\n");
      // Fallback to a benign immediate to avoid UB; you'll see wrong asm,
      // but at least you won't segfault.
      return MCOperand::createImm(0);
  }
}

void MCInstLowering::lower(const MachineInstr *MI, MCInst &OutMI) {
  OutMI.clear();
  OutMI.setOpcode(MI->getOpcode());
  
  for (const MachineOperand &MO : MI->operands()) {
    // Skip implicit operands and metadata
    if (MO.isReg() && MO.isImplicit())
      continue; // printers ignore implicit regs
    if (MO.isMetadata())
      continue; // skip metadata operands
    if (MO.isCFIIndex())
      continue; // skip CFI operands
      
    OutMI.addOperand(lowerOperand(AP, MO));
  }
}
