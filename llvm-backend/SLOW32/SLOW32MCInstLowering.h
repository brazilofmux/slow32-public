
#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32MCINSTLOWERING_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32MCINSTLOWERING_H
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCInst.h"
namespace llvm {
class MCInstLowering {
  AsmPrinter &AP;
public:
  MCInstLowering(AsmPrinter &ap) : AP(ap) {}
  void lower(const MachineInstr *MI, MCInst &OutMI);
};
}
#endif
