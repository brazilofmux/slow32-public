//===- SLOW32MachineFunctionInfo.h - SLOW32 Machine Function Info -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares SLOW32-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32MACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

class SLOW32MachineFunctionInfo : public MachineFunctionInfo {
  /// FrameIndex for the varargs area.
  int VarArgsFrameIndex = 0;
  
  /// Size of the save area used for varargs.
  int VarArgsSaveSize = 0;

public:
  SLOW32MachineFunctionInfo() = default;
  
  SLOW32MachineFunctionInfo(const Function &F, const TargetSubtargetInfo *STI) {}

  MachineFunctionInfo *
  clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
        const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
      const override {
    return DestMF.cloneInfo<SLOW32MachineFunctionInfo>(*this);
  }

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }

  int getVarArgsSaveSize() const { return VarArgsSaveSize; }
  void setVarArgsSaveSize(int Size) { VarArgsSaveSize = Size; }

  // Prologue save decisions for LR (r31) and FP (r30), fixed once in
  // determineCalleeSaves after register allocation so that prologue,
  // epilogue, and frame-index offsets all agree. Unset before PEI.
  std::optional<bool> isLRSaved() const { return LRSaved; }
  void setLRSaved(bool V) { LRSaved = V; }
  std::optional<bool> isFPSaved() const { return FPSaved; }
  void setFPSaved(bool V) { FPSaved = V; }

private:
  std::optional<bool> LRSaved;
  std::optional<bool> FPSaved;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SLOW32_SLOW32MACHINEFUNCTIONINFO_H