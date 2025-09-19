#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32ISELLOWERING_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32ISELLOWERING_H

#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/RuntimeLibcalls.h"

namespace llvm {
namespace SLOW32ISD {
enum NodeType {
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  RET_FLAG,
  BR_CC,  // Conditional branch (BEQ)
  BR_NE,  // Branch not equal (BNE)
  BR_LT,  // Signed less-than
  BR_GE,  // Signed greater-or-equal
  BR_GT,  // Signed greater-than
  BR_LE,  // Signed less-or-equal
  BR_LTU, // Unsigned less-than
  BR_GEU, // Unsigned greater-or-equal
  BR_GTU, // Unsigned greater-than
  BR_LEU, // Unsigned less-or-equal
  CALL,   // Function call
  HI,     // High 20 bits of an address
  LO      // Low 12 bits of an address
};
}
class SLOW32TargetLowering : public TargetLowering {
public:
  SLOW32TargetLowering(const TargetMachine &TM);

  const char *getTargetNodeName(unsigned Opcode) const override;

  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerExternalSymbol(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerJumpTable(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBRCOND(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerROTL(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerROTR(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerI64Shift(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSHL_PARTS(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSRL_PARTS(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSRA_PARTS(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerADDC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerADDE(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSUBC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSUBE(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerConstant(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUADDO       (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUADDO_CARRY (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUSUBO       (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUSUBO_CARRY (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUMUL_LOHI   (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSMUL_LOHI   (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerMULHU       (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerMULHS       (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUDIV        (SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerUREM        (SDValue Op, SelectionDAG &DAG) const;

  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
                               bool isVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &dl, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;

  SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                     const SmallVectorImpl<ISD::OutputArg> &Outs,
                     const SmallVectorImpl<SDValue> &OutVals,
                     const SDLoc &dl, SelectionDAG &DAG) const override;

  SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
                   SmallVectorImpl<SDValue> &InVals) const override;

  // Memory operation optimizations
  EVT getOptimalMemOpType(LLVMContext &Context, const MemOp &Op,
                          const AttributeList &FuncAttributes) const override;

  // Tell LLVM about misaligned memory access support
  bool allowsMisalignedMemoryAccesses(EVT VT, unsigned AddrSpace,
                                      Align Alignment,
                                      MachineMemOperand::Flags Flags = MachineMemOperand::MONone,
                                      unsigned *Fast = nullptr) const override;


  // Addressing mode queries
  bool isLegalAddressingMode(const DataLayout &DL, const AddrMode &AM,
                             Type *Ty, unsigned AS,
                             Instruction *I = nullptr) const override;

  // Custom instruction expansion
  MachineBasicBlock *EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const override;

  // Replace custom node results for type legalization
  void ReplaceNodeResults(SDNode *N, SmallVectorImpl<SDValue> &Results,
                          SelectionDAG &DAG) const override;

  // Inline assembly support
  ConstraintType getConstraintType(StringRef Constraint) const override;
  
  std::pair<unsigned, const TargetRegisterClass *>
  getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                               StringRef Constraint, MVT VT) const override;
  
  void LowerAsmOperandForConstraint(SDValue Op, StringRef Constraint,
                                    std::vector<SDValue> &Ops,
                                    SelectionDAG &DAG) const override;
  
  // Custom DAG combines
  SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;
  
private:
  // Helper functions for DAG combines
  SDValue performADDCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performMULCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performANDCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performORCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performSHLCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performLOADCombine(SDNode *N, DAGCombinerInfo &DCI) const;
  SDValue performSTORECombine(SDNode *N, DAGCombinerInfo &DCI) const;
};
}
#endif
