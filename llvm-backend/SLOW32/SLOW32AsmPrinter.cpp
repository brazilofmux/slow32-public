
#include "SLOW32AsmPrinter.h"
#include "SLOW32.h"
#include "SLOW32MCInstLowering.h"
#include "MCTargetDesc/SLOW32InstPrinter.h"
#include "MCTargetDesc/SLOW32MCExpr.h"
#include "TargetInfo/SLOW32TargetInfo.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Debug.h"
using namespace llvm;

// Expand PseudoLongBR into LUI+ADDI+JALR sequence for the standalone assembler.
// BranchRelaxation emits this pseudo for conditional branches that are out of
// range: it inverts the condition into a short branch over a new block that
// contains this unconditional long jump.
static void emitPseudoLongBR(const MachineInstr *MI, AsmPrinter &AP) {
  Register Scratch = MI->getOperand(0).getReg();
  MCSymbol *TargetSym = MI->getOperand(1).getMBB()->getSymbol();
  const MCExpr *TargetExpr =
      MCSymbolRefExpr::create(TargetSym, AP.OutContext);

  // lui scratch, %hi(target)
  const MCExpr *HiExpr = SLOW32MCExpr::create(SLOW32MCExpr::VK_SLOW32_HI,
                                               TargetExpr, AP.OutContext);
  MCInst LUI;
  LUI.setOpcode(SLOW32::LUI);
  LUI.addOperand(MCOperand::createReg(Scratch));
  LUI.addOperand(MCOperand::createExpr(HiExpr));
  AP.OutStreamer->emitInstruction(LUI, AP.getSubtargetInfo());

  // addi scratch, scratch, %lo(target)
  const MCExpr *LoExpr = SLOW32MCExpr::create(SLOW32MCExpr::VK_SLOW32_LO,
                                               TargetExpr, AP.OutContext);
  MCInst ADDI;
  ADDI.setOpcode(SLOW32::ADDI);
  ADDI.addOperand(MCOperand::createReg(Scratch));
  ADDI.addOperand(MCOperand::createReg(Scratch));
  ADDI.addOperand(MCOperand::createExpr(LoExpr));
  AP.OutStreamer->emitInstruction(ADDI, AP.getSubtargetInfo());

  // jalr r0, scratch, 0
  MCInst JALR;
  JALR.setOpcode(SLOW32::JALR);
  JALR.addOperand(MCOperand::createReg(SLOW32::R0));
  JALR.addOperand(MCOperand::createReg(Scratch));
  AP.OutStreamer->emitInstruction(JALR, AP.getSubtargetInfo());
}

void SLOW32AsmPrinter::emitInstruction(const MachineInstr *MI) {
  if (MI->getOpcode() == SLOW32::PseudoLongBR) {
    emitPseudoLongBR(MI, *this);
    return;
  }

  MCInstLowering Lower(*this);
  MCInst Inst;
  Lower.lower(MI, Inst);
  assert(Inst.getOpcode() != 0 && "Lowered MCInst has opcode 0");
  OutStreamer->emitInstruction(Inst, getSubtargetInfo());
}

void SLOW32AsmPrinter::emitBasicBlockStart(const MachineBasicBlock &MBB) {
  // SLOW32: Override to ensure we always emit proper labels for branch targets.
  // The default implementation sometimes only emits comments like "# %bb.N:"
  // We need actual labels like ".LBBN_M:" for the assembler to resolve branches.

  // Emit blockaddress symbols (.Ltmp0 etc.) for blocks whose address is taken
  // via blockaddress() in the IR.  These are the symbols that computed-goto
  // dispatch tables reference and must appear before the MBB label.
  if (MBB.isIRBlockAddressTaken()) {
    if (isVerbose())
      OutStreamer->AddComment("Block address taken");
    BasicBlock *BB = MBB.getAddressTakenIRBlock();
    for (MCSymbol *Sym : getAddrLabelSymbolToEmit(BB))
      OutStreamer->emitLabel(Sym);
  }

  // Always emit the label if this block has predecessors (is a branch target)
  // or if it's explicitly marked as having its address taken.
  if (!MBB.pred_empty() || MBB.hasAddressTaken()) {
    OutStreamer->emitLabel(MBB.getSymbol());
  } else if (isVerbose()) {
    OutStreamer->emitRawComment(" %bb." + Twine(MBB.getNumber()) + ":",
                                /*TabPrefix=*/false);
  }
}

#include "SLOW32TargetMachine.h"
#include "llvm/MC/TargetRegistry.h"

bool SLOW32AsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                       const char *ExtraCode, raw_ostream &OS) {
  // First try the generic code, which knows about modifiers like 'c' and 'n'.
  if (!AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, OS))
    return false;

  const MachineOperand &MO = MI->getOperand(OpNo);

  // Handle any SLOW32-specific modifiers here
  if (ExtraCode) {
    return true; // Unknown modifier
  }

  // Print the operand
  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    OS << SLOW32InstPrinter::getRegisterName(MO.getReg());
    return false;
  case MachineOperand::MO_Immediate:
    OS << MO.getImm();
    return false;
  case MachineOperand::MO_GlobalAddress:
  case MachineOperand::MO_ExternalSymbol:
    // Print the symbol name
    if (MO.getType() == MachineOperand::MO_GlobalAddress)
      OS << MO.getGlobal()->getName();
    else
      OS << MO.getSymbolName();
    
    // Add any offset
    if (MO.getOffset())
      OS << "+" << MO.getOffset();
    return false;
  default:
    break;
  }

  return true;
}

bool SLOW32AsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                             unsigned OpNo,
                                             const char *ExtraCode,
                                             raw_ostream &OS) {
  // Memory operands are typically printed as "offset(base)"
  // For SLOW32, we'll use the format: [base + offset]
  
  if (ExtraCode)
    return true; // Unknown modifier
    
  const MachineOperand &BaseReg = MI->getOperand(OpNo);
  const MachineOperand &Offset = MI->getOperand(OpNo + 1);
  
  OS << "[";
  OS << SLOW32InstPrinter::getRegisterName(BaseReg.getReg());
  
  if (Offset.isImm() && Offset.getImm() != 0) {
    if (Offset.getImm() > 0)
      OS << " + ";
    else
      OS << " - ";
    OS << std::abs(Offset.getImm());
  } else if (Offset.isGlobal()) {
    OS << " + " << Offset.getGlobal()->getName();
    if (Offset.getOffset())
      OS << " + " << Offset.getOffset();
  }
  
  OS << "]";
  return false;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSLOW32AsmPrinter() {
  RegisterAsmPrinter<SLOW32AsmPrinter> X(getTheSLOW32Target());
}
