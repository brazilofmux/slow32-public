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

using namespace llvm;

#define DEBUG_TYPE "slow32-isel"

namespace {
class SLOW32DAGToDAGISel final : public SelectionDAGISel {
public:
  SLOW32DAGToDAGISel(SLOW32TargetMachine &TM)
      : SelectionDAGISel(TM) {}

  // Select address mode for loads/stores
  // This is critical for proper byte-wise memcpy expansion
  bool SelectAddr(SDValue N, SDValue &Base, SDValue &Offset) {
    SDLoc DL(N);
    
    // Handle GlobalAddress nodes with offsets (critical for memcpy expansion)
    // This handles patterns like .L__const.main.str+1, .L__const.main.str+2, etc.
    if (GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(N)) {
      int64_t GOffset = GA->getOffset();
      
      // Split the GlobalAddress into base address and offset
      // Create a GlobalAddress with offset 0 for the base
      SDValue GABase = CurDAG->getTargetGlobalAddress(
        GA->getGlobal(), DL, MVT::i32, 0, GA->getTargetFlags());
      
      // Generate LUI+ADDI to materialize the base address
      SDValue Hi = SDValue(CurDAG->getMachineNode(SLOW32::LUI, DL, MVT::i32,
                                                  GABase), 0);
      Base = SDValue(CurDAG->getMachineNode(SLOW32::ADDI, DL, MVT::i32,
                                            Hi, GABase), 0);
      
      // The offset from the GlobalAddress
      Offset = CurDAG->getTargetConstant(GOffset, DL, MVT::i32);
      return true;
    }
    
    // Handle (base + constant) addressing
    if (N.getOpcode() == ISD::ADD) {
      // Check if the first operand is a GlobalAddress with an offset
      if (GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(N.getOperand(0))) {
        if (auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
          int64_t CVal = CN->getSExtValue();
          int64_t GOffset = GA->getOffset();
          int64_t TotalOffset = GOffset + CVal;
          
          // Check if total offset fits in 16-bit signed immediate
          if (isInt<16>(TotalOffset)) {
            // Create base address (GlobalAddress with offset 0)
            SDValue GABase = CurDAG->getTargetGlobalAddress(
              GA->getGlobal(), DL, MVT::i32, 0, GA->getTargetFlags());
            
            // Generate LUI+ADDI to materialize the base address
            SDValue Hi = SDValue(CurDAG->getMachineNode(SLOW32::LUI, DL, MVT::i32,
                                                        GABase), 0);
            Base = SDValue(CurDAG->getMachineNode(SLOW32::ADDI, DL, MVT::i32,
                                                  Hi, GABase), 0);
            
            Offset = CurDAG->getTargetConstant(TotalOffset, DL, MVT::i32);
            return true;
          }
        }
      }
      
      // Normal ADD pattern with constant offset
      if (auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
        int64_t CVal = CN->getSExtValue();
        // Check if constant fits in 16-bit signed immediate
        if (isInt<16>(CVal)) {
          Base = N.getOperand(0);
          Offset = CurDAG->getTargetConstant(CVal, DL, MVT::i32);
          return true;
        }
      }
    }
    
    // Handle nested ADD for (HI+LO)+offset patterns
    if (N.getOpcode() == ISD::ADD && N.getOperand(0).getOpcode() == ISD::ADD) {
      if (auto *CN = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
        int64_t CVal = CN->getSExtValue();
        if (isInt<16>(CVal)) {
          Base = N.getOperand(0);
          Offset = CurDAG->getTargetConstant(CVal, DL, MVT::i32);
          return true;
        }
      }
    }
    
    // Handle FrameIndex nodes
    if (auto *FI = dyn_cast<FrameIndexSDNode>(N)) {
      Base = CurDAG->getTargetFrameIndex(FI->getIndex(), MVT::i32);
      Offset = CurDAG->getTargetConstant(0, DL, MVT::i32);
      return true;
    }
    
    // Default case: base with 0 offset
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
    case ISD::Constant: {
      // Handle large constants that don't fit in immediate fields
      auto *CN = cast<ConstantSDNode>(N);
      int64_t Imm = CN->getSExtValue();
      
      // If the constant fits in 12 bits (sign-extended), let normal patterns handle it
      if (Imm >= -2048 && Imm <= 2047) {
        break;
      }
      
      // For large constants, we need LUI + ADDI (RISC-V style)
      SDLoc DL(N);
      EVT VT = N->getValueType(0);
      SDValue Result;
      
      // Split into 20-bit upper and 12-bit lower parts
      uint32_t UImm = (uint32_t)Imm;
      int32_t Lo12 = UImm & 0xFFF;
      int32_t Hi20 = (UImm >> 12) & 0xFFFFF;
      
      // If bit 11 of Lo12 is set, it will be sign-extended by ADDI
      // We need to adjust Hi20 to compensate
      if (Lo12 & 0x800) {
        Hi20 = (Hi20 + 1) & 0xFFFFF;  // Increment and mask to 20 bits
        // Keep Lo12 as the original 12-bit value for the instruction encoding
        // ADDI will sign-extend it at runtime
      }
      
      if (Hi20 == 0 && !(Lo12 & 0x800)) {
        // Just ADDI is enough (small positive constant that fits in 12 bits)
        Result = SDValue(CurDAG->getMachineNode(SLOW32::ADDI, DL, VT,
                                       CurDAG->getRegister(SLOW32::R0, VT),
                                       CurDAG->getTargetConstant(Lo12, DL, VT)), 0);
      } else if (Lo12 == 0) {
        // Just LUI is enough (lower 12 bits are zero)
        Result = SDValue(CurDAG->getMachineNode(SLOW32::LUI, DL, VT,
                                       CurDAG->getTargetConstant(Hi20, DL, VT)), 0);
      } else {
        // Need both LUI and ADDI
        SDValue HiNode = SDValue(CurDAG->getMachineNode(SLOW32::LUI, DL, VT,
                                    CurDAG->getTargetConstant(Hi20, DL, VT)), 0);
        Result = SDValue(CurDAG->getMachineNode(SLOW32::ADDI, DL, VT,
                                       HiNode,
                                       CurDAG->getTargetConstant(Lo12, DL, VT)), 0);
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
