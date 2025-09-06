//===-- SLOW32TargetMachine.h - Define TargetMachine for SLOW32 -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32TARGETMACHINE_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32TARGETMACHINE_H

#include "SLOW32Subtarget.h"
#include "llvm/CodeGen/CodeGenTargetMachineImpl.h"
#include <memory>

namespace llvm {

class SLOW32TargetMachine : public CodeGenTargetMachineImpl {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  SLOW32Subtarget Subtarget;

public:
  SLOW32TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      std::optional<Reloc::Model> RM,
                      std::optional<CodeModel::Model> CM,
                      CodeGenOptLevel OL, bool JIT);
                      
  const SLOW32Subtarget *getSubtargetImpl(const Function &F) const override {
    return &Subtarget;
  }
  
  const SLOW32Subtarget &getSubtarget() const { return Subtarget; }
  
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
  
  MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                           const TargetSubtargetInfo *STI) const override;
};

} // end namespace llvm

#endif