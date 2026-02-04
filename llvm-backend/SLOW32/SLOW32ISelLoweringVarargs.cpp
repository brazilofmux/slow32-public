//===-- SLOW32ISelLoweringVarargs.cpp - Varargs Lowering -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32ISelLowering.h"
#include "SLOW32MachineFunctionInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/Support/Alignment.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-lower"

SDValue SLOW32TargetLowering::LowerVASTART(SDValue Op,
                                           SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  SLOW32MachineFunctionInfo *FuncInfo = MF.getInfo<SLOW32MachineFunctionInfo>();

  SDLoc DL(Op);
  SDValue FI = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                                 getPointerTy(MF.getDataLayout()));

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), DL, FI, Op.getOperand(1),
                      MachinePointerInfo(SV));
}

SDValue SLOW32TargetLowering::LowerVAARG(SDValue Op,
                                         SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT RetVT = Op.getValueType();        // type the caller requested (i8/i16/i32/i64/ptr)
  SDValue Chain = Op.getOperand(0);
  SDValue VAListPtr = Op.getOperand(1); // i32* pointing to current AP

  const DataLayout &DLay = DAG.getDataLayout();
  EVT PtrVT = getPointerTy(DLay);

  // Bail out to the generic expander for types we do not explicitly support
  // yet. This keeps the compiler from asserting until SLOW32 grows the
  // required ABI support.
  bool IsPtr = (RetVT == PtrVT);
  bool IsI64 = (RetVT == MVT::i64);
  bool IsSmallInt = RetVT.isInteger() && RetVT.getSizeInBits() <= 32 && RetVT != MVT::i64;
  bool IsF32 = (RetVT == MVT::f32);
  bool IsF64 = (RetVT == MVT::f64);

  if (!IsI64 && !IsPtr && !IsSmallInt && !IsF32 && !IsF64) {
    SDValue Expanded = DAG.expandVAArg(Op.getNode());
    return DAG.getMergeValues({Expanded, Expanded.getValue(1)}, DL);
  }

  // 1) Load current AP from *va_list
  //    (the VASTART implementation stored the address of the varargs area here)
  MachinePointerInfo MPIList; // unknown for now; fine for stack/locals
  SDValue AP = DAG.getLoad(PtrVT, DL, Chain, VAListPtr, MPIList, Align(4));
  Chain = AP.getValue(1);

  // 2) Compute the size to fetch from the slot according to default promotions
  unsigned FetchBytes = 4;         // default for all ≤32-bit integer/pointer types
  if (IsI64 || IsF64)
    FetchBytes = 8;

  // 3) Perform the load(s)
  SDValue Val;
  if (IsI64 || IsF64) {
    // Load lo/hi 32-bit words and build i64
    SDValue Lo = DAG.getLoad(MVT::i32, DL, Chain, AP, MPIList, Align(4));
    Chain = Lo.getValue(1);

    SDValue APPlus4 = DAG.getNode(ISD::ADD, DL, PtrVT, AP,
                                   DAG.getConstant(4, DL, PtrVT));
    SDValue Hi = DAG.getLoad(MVT::i32, DL, Chain, APPlus4, MPIList, Align(4));
    Chain = Hi.getValue(1);

    if (IsF64)
      Val = DAG.getNode(SLOW32ISD::BuildPairF64, DL, MVT::f64, Lo, Hi);
    else
      Val = DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i64, Lo, Hi);
  } else {
    // Always load the full 32-bit promoted slot
    SDValue W = DAG.getLoad(MVT::i32, DL, Chain, AP, MPIList, Align(4));
    Chain = W.getValue(1);

    if (RetVT == MVT::i32 || IsPtr)
      Val = W;
    else if (IsF32)
      Val = DAG.getNode(ISD::BITCAST, DL, RetVT, W);
    else if (RetVT.getSizeInBits() < 32)
      Val = DAG.getNode(ISD::TRUNCATE, DL, RetVT, W);
  }

  // 4) Bump AP and store it back into *va_list
  SDValue Bump = DAG.getConstant(FetchBytes, DL, PtrVT);
  SDValue NextAP = DAG.getNode(ISD::ADD, DL, PtrVT, AP, Bump);
  Chain = DAG.getStore(Chain, DL, NextAP, VAListPtr, MPIList, Align(4));

  // 5) Return (value, chain)
  return DAG.getMergeValues({Val, Chain}, DL);
}
