//===-- SLOW32ISelLoweringCombines.cpp - DAG Combine Helpers -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32ISelLowering.h"
#include "llvm/CodeGen/SelectionDAG.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-lower"

SDValue SLOW32TargetLowering::PerformDAGCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  switch (N->getOpcode()) {
  default:
    break;
  case ISD::ADD:
    return performADDCombine(N, DCI);
  case ISD::MUL:
    return performMULCombine(N, DCI);
  case ISD::AND:
    return performANDCombine(N, DCI);
  case ISD::OR:
    return performORCombine(N, DCI);
  case ISD::SHL:
    return performSHLCombine(N, DCI);
  case ISD::LOAD:
    return performLOADCombine(N, DCI);
  case ISD::STORE:
    return performSTORECombine(N, DCI);
  }
  return SDValue();
}

SDValue SLOW32TargetLowering::performADDCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  SDLoc DL(N);

  SDValue N0 = N->getOperand(0);
  SDValue N1 = N->getOperand(1);

  if (!DCI.isBeforeLegalize())
    return SDValue();

  if (N0.getOpcode() == ISD::SHL && N0.getOperand(0) == N1) {
    if (auto *C = dyn_cast<ConstantSDNode>(N0.getOperand(1))) {
      unsigned ShAmt = C->getZExtValue();
      if (ShAmt >= 1 && ShAmt <= 3) {
        unsigned MulConst = (1U << ShAmt) + 1;
        return DAG.getNode(ISD::MUL, DL, N->getValueType(0), N1,
                           DAG.getConstant(MulConst, DL, N->getValueType(0)));
      }
    }
  }

  if (N1.getOpcode() == ISD::SHL && N1.getOperand(0) == N0) {
    if (auto *C = dyn_cast<ConstantSDNode>(N1.getOperand(1))) {
      unsigned ShAmt = C->getZExtValue();
      if (ShAmt >= 1 && ShAmt <= 3) {
        unsigned MulConst = (1U << ShAmt) + 1;
        return DAG.getNode(ISD::MUL, DL, N->getValueType(0), N0,
                           DAG.getConstant(MulConst, DL, N->getValueType(0)));
      }
    }
  }

  return SDValue();
}

SDValue SLOW32TargetLowering::performMULCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  SDLoc DL(N);

  auto *C = dyn_cast<ConstantSDNode>(N->getOperand(1));
  if (!C)
    return SDValue();

  uint64_t MulAmt = C->getZExtValue();

  if (DCI.isBeforeLegalize() && isPowerOf2_64(MulAmt)) {
    unsigned ShAmt = Log2_64(MulAmt);
    return DAG.getNode(ISD::SHL, DL, N->getValueType(0), N->getOperand(0),
                       DAG.getConstant(ShAmt, DL, MVT::i32));
  }

  if (!DCI.isAfterLegalizeDAG())
    return SDValue();

  if (MulAmt == 3 || MulAmt == 5 || MulAmt == 9) {
    unsigned ShAmt = (MulAmt == 3) ? 1 : (MulAmt == 5) ? 2 : 3;
    SDValue Shl = DAG.getNode(ISD::SHL, DL, N->getValueType(0), N->getOperand(0),
                              DAG.getConstant(ShAmt, DL, MVT::i32));
    return DAG.getNode(ISD::ADD, DL, N->getValueType(0), Shl, N->getOperand(0));
  }

  if (MulAmt == 6) {
    SDValue Shl1 = DAG.getNode(ISD::SHL, DL, N->getValueType(0), N->getOperand(0),
                               DAG.getConstant(1, DL, MVT::i32));
    SDValue Add = DAG.getNode(ISD::ADD, DL, N->getValueType(0), Shl1,
                              N->getOperand(0));
    return DAG.getNode(ISD::SHL, DL, N->getValueType(0), Add,
                       DAG.getConstant(1, DL, MVT::i32));
  }

  return SDValue();
}

SDValue SLOW32TargetLowering::performANDCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  auto *C = dyn_cast<ConstantSDNode>(N->getOperand(1));
  if (!C)
    return SDValue();

  uint64_t AndMask = C->getZExtValue();

  if (AndMask == 0xFFFF && N->getOperand(0).getOpcode() == ISD::SRL) {
    if (auto *ShC = dyn_cast<ConstantSDNode>(N->getOperand(0).getOperand(1)))
      if (ShC->getZExtValue() == 16)
        return N->getOperand(0);
  }

  if (AndMask == 1 && N->hasOneUse()) {
    SDNode *Use = N->use_begin()->getUser();
    if (Use->getOpcode() == ISD::BRCOND || Use->getOpcode() == ISD::BR_CC)
      return SDValue();
  }

  return SDValue();
}

SDValue SLOW32TargetLowering::performORCombine(SDNode *N,
                                               DAGCombinerInfo &DCI) const {
  SDValue N0 = N->getOperand(0);
  SDValue N1 = N->getOperand(1);

  if (N0.getOpcode() == ISD::SHL && N1.getOpcode() == ISD::AND) {
    if (auto *ShC = dyn_cast<ConstantSDNode>(N0.getOperand(1)))
      if (auto *AndC = dyn_cast<ConstantSDNode>(N1.getOperand(1)))
        if (ShC->getZExtValue() == 16 && AndC->getZExtValue() == 0xFFFF)
          return SDValue();
  }

  return SDValue();
}

SDValue SLOW32TargetLowering::performSHLCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  SDLoc DL(N);

  if (N->getOperand(0).getOpcode() == ISD::SHL) {
    if (auto *C1 = dyn_cast<ConstantSDNode>(N->getOperand(0).getOperand(1)))
      if (auto *C2 = dyn_cast<ConstantSDNode>(N->getOperand(1))) {
        unsigned ShAmt = C1->getZExtValue() + C2->getZExtValue();
        if (ShAmt < 32)
          return DAG.getNode(ISD::SHL, DL, N->getValueType(0),
                             N->getOperand(0).getOperand(0),
                             DAG.getConstant(ShAmt, DL, MVT::i32));
      }
  }

  return SDValue();
}

SDValue SLOW32TargetLowering::performLOADCombine(SDNode *N,
                                                 DAGCombinerInfo &DCI) const {
  auto *LD = cast<LoadSDNode>(N);
  if (LD->isVolatile())
    return SDValue();
  return SDValue();
}

SDValue SLOW32TargetLowering::performSTORECombine(SDNode *N,
                                                  DAGCombinerInfo &DCI) const {
  auto *ST = cast<StoreSDNode>(N);
  if (ST->isVolatile())
    return SDValue();
  return SDValue();
}
