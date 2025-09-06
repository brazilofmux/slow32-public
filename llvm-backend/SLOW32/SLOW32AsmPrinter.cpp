
#include "SLOW32AsmPrinter.h"
#include "SLOW32MCInstLowering.h"
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

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSLOW32AsmPrinter() {
  RegisterAsmPrinter<SLOW32AsmPrinter> X(getTheSLOW32Target());
}
