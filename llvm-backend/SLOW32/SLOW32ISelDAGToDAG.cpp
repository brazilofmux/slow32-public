//===-- SLOW32ISelDAGToDAG.cpp - SLOW32 DAG to DAG Instruction Selector --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32.h"
#include "SLOW32ISelLowering.h"
#include "SLOW32TargetMachine.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-isel"

namespace {
class SLOW32DAGToDAGISel final : public SelectionDAGISel {
public:
  SLOW32DAGToDAGISel(SLOW32TargetMachine &TM)
      : SelectionDAGISel(TM) {}

private:
  SDValue createLoadAddrForGlobal(const SDLoc &DL,
                                  GlobalAddressSDNode *GA,
                                  int64_t AdditionalOffset) const {
    int64_t TotalOffset = GA->getOffset() + AdditionalOffset;
    SDValue BaseSym = CurDAG->getTargetGlobalAddress(
        GA->getGlobal(), DL, MVT::i32, TotalOffset, GA->getTargetFlags());
    SDNode *LoadAddr =
        CurDAG->getMachineNode(SLOW32::LOAD_ADDR, DL, MVT::i32, BaseSym);
    return SDValue(LoadAddr, 0);
  }

  SDValue tryFoldLoadAddrAddend(SDValue LoadAddr, int64_t ExtraOffset,
                                const SDLoc &DL) const {
    SDNode *LoadAddrNode = LoadAddr.getNode();
    if (!LoadAddrNode || !LoadAddrNode->isMachineOpcode() ||
        LoadAddrNode->getMachineOpcode() != SLOW32::LOAD_ADDR)
      return SDValue();

    SDValue SymbolOp = LoadAddrNode->getOperand(0);
    if (auto *GA = dyn_cast<GlobalAddressSDNode>(SymbolOp.getNode()))
      return createLoadAddrForGlobal(DL, GA, ExtraOffset);

    return SDValue();
  }

public:
  // Select address mode for loads/stores
  // This is critical for proper byte-wise memcpy expansion
  bool SelectAddr(SDValue N, SDValue &Base, SDValue &Offset) {
    SDLoc DL(N);

    if (auto *GA = dyn_cast<GlobalAddressSDNode>(N)) {
      Base = createLoadAddrForGlobal(DL, GA, /*AdditionalOffset=*/0);
      Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
      return true;
    }

    if (N.getOpcode() == ISD::ADD) {
      if (auto *GA = dyn_cast<GlobalAddressSDNode>(N.getOperand(0))) {
        if (const auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
          Base = createLoadAddrForGlobal(DL, GA, CN->getSExtValue());
          Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
          return true;
        }
      }
      if (auto *GA = dyn_cast<GlobalAddressSDNode>(N.getOperand(1))) {
        if (const auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(0))) {
          Base = createLoadAddrForGlobal(DL, GA, CN->getSExtValue());
          Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
          return true;
        }
      }

      if (const auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
        if (SDValue Folded = tryFoldLoadAddrAddend(N.getOperand(0),
                                                   CN->getSExtValue(), DL)) {
          Base = Folded;
          Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
          return true;
        }

        int64_t CVal = CN->getSExtValue();
        if (isInt<12>(CVal)) {
          Base = N.getOperand(0);
          Offset = CurDAG->getTargetConstant(CVal, DL, MVT::i32);
          return true;
        }
      }

      if (const auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(0))) {
        if (SDValue Folded = tryFoldLoadAddrAddend(N.getOperand(1),
                                                   CN->getSExtValue(), DL)) {
          Base = Folded;
          Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
          return true;
        }

        int64_t CVal = CN->getSExtValue();
        if (isInt<12>(CVal)) {
          Base = N.getOperand(1);
          Offset = CurDAG->getTargetConstant(CVal, DL, MVT::i32);
          return true;
        }
      }
    }

    if (auto *FI = dyn_cast<FrameIndexSDNode>(N)) {
      Base = CurDAG->getTargetFrameIndex(FI->getIndex(), MVT::i32);
      Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
      return true;
    }

    Base = N;
    Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
    return true;
  }

  void Select(SDNode *N) override {
    // Handle custom nodes
    if (N->isMachineOpcode()) {
      N->setNodeId(-1);
      return;
    }
    
    switch (N->getOpcode()) {
    case ISD::ADD: {
      SDLoc DL(N);
      SDValue Op0 = N->getOperand(0);
      SDValue Op1 = N->getOperand(1);

      if (auto *GA = dyn_cast<GlobalAddressSDNode>(Op0)) {
        if (const auto *CN = dyn_cast<ConstantSDNode>(Op1)) {
          SDValue Folded =
              createLoadAddrForGlobal(DL, GA, CN->getSExtValue());
          ReplaceNode(N, Folded.getNode());
          return;
        }
      }
      if (auto *GA = dyn_cast<GlobalAddressSDNode>(Op1)) {
        if (const auto *CN = dyn_cast<ConstantSDNode>(Op0)) {
          SDValue Folded =
              createLoadAddrForGlobal(DL, GA, CN->getSExtValue());
          ReplaceNode(N, Folded.getNode());
          return;
        }
      }

      if (const auto *CN = dyn_cast<ConstantSDNode>(Op1)) {
        if (SDValue Folded =
                tryFoldLoadAddrAddend(Op0, CN->getSExtValue(), DL)) {
          ReplaceNode(N, Folded.getNode());
          return;
        }
      }
      if (const auto *CN = dyn_cast<ConstantSDNode>(Op0)) {
        if (SDValue Folded =
                tryFoldLoadAddrAddend(Op1, CN->getSExtValue(), DL)) {
          ReplaceNode(N, Folded.getNode());
          return;
        }
      }

      break;
    }
    case ISD::Constant: {
      // Handle large constants that don't fit in immediate fields
      auto *CN = cast<ConstantSDNode>(N);
      int64_t Imm = CN->getSExtValue();
      
      // If the constant fits in 12 bits (sign-extended), let normal patterns handle it
      if (Imm >= -2048 && Imm <= 2047) {
        break;
      }
      
      // For large constants, we need LUI + ADDI (RISC-V style rounding)
      SDLoc DL(N);
      EVT VT = N->getValueType(0);

      int64_t Hi = (Imm + 0x800) >> 12;
      int32_t Lo = Imm - static_cast<int64_t>(Hi << 12);
      assert(isInt<12>(Lo) && "Lo12 out of range after materialisation");

      SDValue Result;

      uint32_t HiBits = static_cast<uint32_t>(Hi) & 0xFFFFF;
      SDValue HiImm = CurDAG->getTargetConstant(HiBits, DL, MVT::i32);
      APInt LoAP(32, static_cast<uint64_t>(static_cast<int64_t>(Lo)), true);
      SDValue LoImm = CurDAG->getTargetConstant(LoAP, DL, MVT::i32);

      if (Hi == 0) {
        // Pure ADDI from zero (covers both positive and negative 12-bit values)
        Result = SDValue(CurDAG->getMachineNode(SLOW32::ADDI, DL, VT,
                                       CurDAG->getRegister(SLOW32::R0, VT),
                                       LoImm),
                         0);
      } else if (Lo == 0) {
        Result = SDValue(
            CurDAG->getMachineNode(SLOW32::LUI, DL, VT, HiImm), 0);
      } else {
        SDValue HiNode =
            SDValue(CurDAG->getMachineNode(SLOW32::LUI, DL, VT, HiImm), 0);
        Result = SDValue(
            CurDAG->getMachineNode(SLOW32::ADDI, DL, VT, HiNode, LoImm), 0);
      }
      
      ReplaceNode(N, Result.getNode());
      return;
    }
    case SLOW32ISD::BR_CC:
      // Let TableGen patterns handle branch selection
      break;
    case SLOW32ISD::CALL: {
      // Handle CALL node selection manually to deal with variable_ops properly
      SDLoc DL(N);
      SDValue Callee = N->getOperand(1);
      
      // Build operands for the call instruction
      SmallVector<SDValue, 8> Ops;
      Ops.push_back(Callee);

      // Preserve the call-preserved register mask if present.
      if (N->getNumOperands() > 2 &&
          N->getOperand(2).getOpcode() == ISD::RegisterMask) {
        Ops.push_back(N->getOperand(2));
      }
      
      // Walk the glue chain to find argument registers that need to be marked as implicit uses
      SmallVector<unsigned, 8> ArgRegs;
      if (N->getNumOperands() > 2 && 
          N->getOperand(N->getNumOperands()-1).getValueType() == MVT::Glue) {
        SDValue GlueIn = N->getOperand(N->getNumOperands()-1);
        
        // Walk backwards through the glue chain looking for CopyToReg nodes
        SDNode *Current = GlueIn.getNode();
        while (Current && Current->getOpcode() == ISD::CopyToReg) {
          if (Current->getNumOperands() >= 2) {
            if (auto *RegNode = dyn_cast<RegisterSDNode>(Current->getOperand(1))) {
              unsigned Reg = RegNode->getReg();
              // Check if this is an argument register (R3-R10)
              if (Reg >= SLOW32::R3 && Reg <= SLOW32::R10) {
                ArgRegs.push_back(Reg);
              }
            }
          }
          // Move to the glue input of this CopyToReg (if it exists)
          if (Current->getNumOperands() >= 4 && 
              Current->getOperand(3).getValueType() == MVT::Glue) {
            Current = Current->getOperand(3).getNode();
          } else {
            break;
          }
        }
        
        // Add implicit register uses for argument registers
        // These need to be added as SDValue register operands before chain/glue
        for (unsigned Reg : ArgRegs) {
          Ops.push_back(CurDAG->getRegister(Reg, MVT::i32));
        }
      }
      
      // Add chain (must be after regular operands, before glue)
      Ops.push_back(N->getOperand(0));
      
      // Add glue if present (must be last)
      if (N->getNumOperands() > 2 && 
          N->getOperand(N->getNumOperands()-1).getValueType() == MVT::Glue) {
        Ops.push_back(N->getOperand(N->getNumOperands()-1));
      }
      
      // Determine which call instruction to use
      unsigned Opcode;
      if (Callee.getOpcode() == ISD::TargetGlobalAddress || 
          Callee.getOpcode() == ISD::TargetExternalSymbol) {
        Opcode = SLOW32::JAL_CALL;
      } else {
        Opcode = SLOW32::JALR_CALL;
      }
      
      // Create the machine node
      MachineSDNode *CallNode = CurDAG->getMachineNode(Opcode, DL,
                                                       MVT::Other, MVT::Glue, Ops);
      
      // Replace the node
      ReplaceNode(N, CallNode);
      return;
    }
    case SLOW32ISD::RET_FLAG: {
      // Select between RET and RET64 based on whether R2 is live
      // Look at the glue chain to see if R2 was set
      SDLoc DL(N);
      bool UsesR2 = false;
      
      // Check if there's a glue input
      if (N->getNumOperands() > 1 && 
          N->getOperand(N->getNumOperands()-1).getValueType() == MVT::Glue) {
        SDValue GlueIn = N->getOperand(N->getNumOperands()-1);
        
        // Walk the glue chain looking for CopyToReg to R2
        SDNode *Current = GlueIn.getNode();
        while (Current && Current->getOpcode() == ISD::CopyToReg) {
          if (Current->getNumOperands() >= 2) {
            if (auto *RegNode = dyn_cast<RegisterSDNode>(Current->getOperand(1))) {
              if (RegNode->getReg() == SLOW32::R2) {
                UsesR2 = true;
                break;
              }
            }
          }
          // Move to the chain input of this CopyToReg
          if (Current->getNumOperands() >= 4 && 
              Current->getOperand(3).getValueType() == MVT::Glue) {
            Current = Current->getOperand(3).getNode();
          } else {
            break;
          }
        }
      }
      
      // Select the appropriate return instruction
      unsigned Opcode = UsesR2 ? SLOW32::RET64 : SLOW32::RET;
      SmallVector<SDValue, 2> Ops;
      for (unsigned i = 0; i < N->getNumOperands(); ++i) {
        Ops.push_back(N->getOperand(i));
      }
      
      MachineSDNode *RetNode = CurDAG->getMachineNode(Opcode, DL, MVT::Other, Ops);
      ReplaceNode(N, RetNode);
      return;
    }
    case ISD::FrameIndex: {
      // Convert FrameIndex to an ADDI that will be fixed up later
      SDLoc DL(N);
      int FI = cast<FrameIndexSDNode>(N)->getIndex();
      
      // Create ADDI with FP and the frame index as an immediate
      // eliminateFrameIndex will fix this up with the real offset
      SDNode *Result = CurDAG->getMachineNode(SLOW32::ADDI, DL, MVT::i32,
                                              CurDAG->getRegister(SLOW32::R30, MVT::i32),
                                              CurDAG->getTargetFrameIndex(FI, MVT::i32));
      
      ReplaceNode(N, Result);
      return;
    }
    // Let standard patterns handle GlobalAddress
    // It should be converted to TargetGlobalAddress by lowering
    // and then matched by TableGen patterns
    default:
      break;
    }
    
    // Let TableGen handle everything else
    SelectCode(N);
  }

  #include "SLOW32GenDAGISel.inc"
};
} // end anonymous namespace

// Need to wrap in a legacy pass
namespace {
class SLOW32DAGToDAGISelLegacy : public SelectionDAGISelLegacy {
public:
  static char ID;
  SLOW32DAGToDAGISelLegacy(SLOW32TargetMachine &TM)
      : SelectionDAGISelLegacy(ID, std::make_unique<SLOW32DAGToDAGISel>(TM)) {}
};
char SLOW32DAGToDAGISelLegacy::ID = 0;
}

FunctionPass *llvm::createSLOW32ISelDag(SLOW32TargetMachine &TM) {
  return new SLOW32DAGToDAGISelLegacy(TM);
}
