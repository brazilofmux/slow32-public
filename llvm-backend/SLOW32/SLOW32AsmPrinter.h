
#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32ASMPRINTER_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32ASMPRINTER_H
#include "llvm/CodeGen/AsmPrinter.h"
namespace llvm {
class SLOW32AsmPrinter : public AsmPrinter {
public:
  SLOW32AsmPrinter(TargetMachine &TM, std::unique_ptr<MCStreamer> Streamer)
    : AsmPrinter(TM, std::move(Streamer)) {}
  void emitInstruction(const MachineInstr *MI) override;
  void emitBasicBlockStart(const MachineBasicBlock &MBB) override;
};
}
#endif
