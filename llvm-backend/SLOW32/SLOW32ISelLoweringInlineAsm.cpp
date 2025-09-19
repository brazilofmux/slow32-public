//===-- SLOW32ISelLoweringInlineAsm.cpp - Inline Asm Helpers -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32ISelLowering.h"
#include "SLOW32.h"
#include "SLOW32RegisterInfo.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "slow32-lower"

TargetLowering::ConstraintType
SLOW32TargetLowering::getConstraintType(StringRef Constraint) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    default:
      break;
    case 'r':
      return C_RegisterClass;
    case 'i':
    case 'n':
      return C_Immediate;
    case 'm':
      return C_Memory;
    }
  }
  return TargetLowering::getConstraintType(Constraint);
}

std::pair<unsigned, const TargetRegisterClass *>
SLOW32TargetLowering::getRegForInlineAsmConstraint(
    const TargetRegisterInfo *TRI, StringRef Constraint, MVT VT) const {
  const TargetRegisterClass *GPRRC =
      TRI ? TRI->getRegClass(SLOW32::GPRRegClassID) : nullptr;

  // First handle single-character constraints
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      // General purpose register for any integer type
      if (GPRRC && (VT.isInteger() || VT == MVT::Other))
        return std::make_pair(0U, GPRRC);
      break;
    default:
      break;
    }
  }

  // Handle specific register names like {r1}, {r2}, etc.
  if (Constraint.size() > 1 && Constraint[0] == '{' &&
      Constraint.back() == '}') {
    StringRef RegName = Constraint.slice(1, Constraint.size() - 1);

    // Try to parse as r0-r31
    if (RegName.starts_with("r")) {
      unsigned RegNum;
      if (!RegName.substr(1).getAsInteger(10, RegNum) && RegNum < 32) {
        // Map to the corresponding register
        unsigned Reg = SLOW32::R0 + RegNum;
        if (GPRRC)
          return std::make_pair(Reg, GPRRC);
      }
    }

    // Handle register aliases
    unsigned Reg = StringSwitch<unsigned>(RegName.lower())
                      .Case("zero", SLOW32::R0)
                      .Case("rv", SLOW32::R1)
                      .Case("t0", SLOW32::R2)
                      .Case("a0", SLOW32::R3)
                      .Case("a1", SLOW32::R4)
                      .Case("a2", SLOW32::R5)
                      .Case("a3", SLOW32::R6)
                      .Case("a4", SLOW32::R7)
                      .Case("a5", SLOW32::R8)
                      .Case("a6", SLOW32::R9)
                      .Case("a7", SLOW32::R10)
                      .Case("sp", SLOW32::R29)
                      .Case("fp", SLOW32::R30)
                      .Case("lr", SLOW32::R31)
                      .Case("ra", SLOW32::R31)
                      .Default(0);

    if (Reg && GPRRC)
      return std::make_pair(Reg, GPRRC);
  }

  // Fall back to the default implementation
  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}

void SLOW32TargetLowering::LowerAsmOperandForConstraint(
    SDValue Op, StringRef Constraint, std::vector<SDValue> &Ops,
    SelectionDAG &DAG) const {
  // Handle 'i' and 'n' immediate constraints
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'i': // Simple integer immediate
    case 'n': // Integer immediate for "n"umerics
      if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
        int64_t Value = C->getSExtValue();
        if (isInt<12>(Value)) {
          Ops.push_back(DAG.getTargetConstant(Value, SDLoc(Op), MVT::i32));
          return;
        }
      }
      break;
    default:
      break;
    }
  }

  // Fall back to default implementation
  TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}
