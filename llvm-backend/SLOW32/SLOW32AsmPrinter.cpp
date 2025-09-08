
#include "SLOW32AsmPrinter.h"
#include "SLOW32MCInstLowering.h"
#include "MCTargetDesc/SLOW32InstPrinter.h"
#include "TargetInfo/SLOW32TargetInfo.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Debug.h"
using namespace llvm;
void SLOW32AsmPrinter::emitInstruction(const MachineInstr *MI) {
  MCInstLowering Lower(*this);
  MCInst Inst;
  Lower.lower(MI, Inst);
  assert(Inst.getOpcode() != 0 && "Lowered MCInst has opcode 0");
  OutStreamer->emitInstruction(Inst, getSubtargetInfo());
}

void SLOW32AsmPrinter::emitBasicBlockStart(const MachineBasicBlock &MBB) {
  // SLOW32: Override to ensure we always emit proper labels for branch targets
  // The default implementation sometimes only emits comments like "# %bb.N:"
  // We need actual labels like ".LBBN_M:" for the assembler to resolve branches
  
  // Don't call parent - we'll handle it ourselves to avoid duplicate labels
  
  // Always emit the label if this block has predecessors (is a branch target)
  // or if it's explicitly marked as having its address taken
  if (!MBB.pred_empty() || MBB.hasAddressTaken()) {
    // Emit the label
    OutStreamer->emitLabel(MBB.getSymbol());
  } else if (isVerbose()) {
    // Only emit a comment for blocks that aren't branch targets
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
