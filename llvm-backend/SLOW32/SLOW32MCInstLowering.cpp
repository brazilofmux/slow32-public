//===-- SLOW32MCInstLowering.cpp -----------------------------------------===//
#include "SLOW32MCInstLowering.h"
#include "SLOW32.h"
#include "MCTargetDesc/SLOW32MCExpr.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "slow32-mcinst-lower"

using namespace llvm;

// Wrap an MC expression with %hi/%lo when the MachineOperand carries those
// target flags. Shared by every symbolic operand kind so long-branch
// materialisation (MBB + MO_HI/MO_LO) stays consistent with globals.
static const MCExpr *applyHiLoFlags(const MCExpr *Expr,
                                    const MachineOperand &MO,
                                    MCContext &Ctx) {
  unsigned TargetFlags = MO.getTargetFlags();
  if (TargetFlags == SLOW32II::MO_HI)
    return SLOW32MCExpr::create(SLOW32MCExpr::VK_SLOW32_HI, Expr, Ctx);
  if (TargetFlags == SLOW32II::MO_LO)
    return SLOW32MCExpr::create(SLOW32MCExpr::VK_SLOW32_LO, Expr, Ctx);
  return Expr;
}

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
      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }

      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
    }
    case MachineOperand::MO_ExternalSymbol: {
      // ExternalSymbol has no offset field.
      MCSymbol *Sym = AP.GetExternalSymbolSymbol(MO.getSymbolName());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
    }
    case MachineOperand::MO_MachineBasicBlock: {
      // Long-branch expansion stamps MO_HI/MO_LO on MBB operands for the
      // LUI/ADDI materialisation sequence. Dropping those flags here produced
      // bare labels (`lui r2, .LBB0_1`) that the external assembler rejects.
      MCSymbol *Sym = MO.getMBB()->getSymbol();
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
    }
    case MachineOperand::MO_JumpTableIndex: {
      // JumpTableIndex has no offset field.
      MCSymbol *Sym = AP.GetJTISymbol(MO.getIndex());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);
      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
    }
    case MachineOperand::MO_ConstantPoolIndex: {
      MCSymbol *Sym = AP.GetCPISymbol(MO.getIndex());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);

      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }

      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
    }
    case MachineOperand::MO_BlockAddress: {
      MCSymbol *Sym = AP.GetBlockAddressSymbol(MO.getBlockAddress());
      const MCExpr *Expr = MCSymbolRefExpr::create(Sym, AP.OutContext);

      int64_t Offset = MO.getOffset();
      if (Offset != 0) {
        const MCExpr *OffsetExpr = MCConstantExpr::create(Offset, AP.OutContext);
        Expr = MCBinaryExpr::createAdd(Expr, OffsetExpr, AP.OutContext);
      }

      return MCOperand::createExpr(applyHiLoFlags(Expr, MO, AP.OutContext));
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
