//===-- SLOW32TargetMachine.cpp - Define TargetMachine for SLOW32 -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32TargetMachine.h"
#include "SLOW32.h"
#include "SLOW32MachineFunctionInfo.h"
#include "TargetInfo/SLOW32TargetInfo.h"
#include "llvm/CodeGen/CodeGenTargetMachineImpl.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSLOW32Target() {
  RegisterTargetMachine<SLOW32TargetMachine> X(getTheSLOW32Target());
  initializeSLOW32LoadAddrOptPass(*PassRegistry::getPassRegistry());
}

static std::string computeDataLayout() {
  // Match the data layout from clang/lib/Basic/Targets/SLOW32.h
  // i64:32:32 = i64 has 32-bit alignment (no native 64-bit support)
  return "e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-"
         "f32:32:32-f64:32:32-n8:16:32-S32";
}

SLOW32TargetMachine::SLOW32TargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           std::optional<Reloc::Model> RM,
                                           std::optional<CodeModel::Model> CM,
                                           CodeGenOptLevel OL, bool JIT)
    : CodeGenTargetMachineImpl(T, computeDataLayout(), TT, CPU, FS, Options,
                               RM.value_or(Reloc::Static),
                               CM.value_or(CodeModel::Small), OL),
      TLOF(std::make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this) {
  initAsmInfo();
}

namespace {
class SLOW32PassConfig : public TargetPassConfig {
public:
  SLOW32PassConfig(SLOW32TargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  SLOW32TargetMachine &getSLOW32TargetMachine() const {
    return getTM<SLOW32TargetMachine>();
  }

  bool addInstSelector() override;
  void addPreRegAlloc() override;

  void addMachineSSAOptimization() override {
    TargetPassConfig::addMachineSSAOptimization();
  }
};
} // end anonymous namespace

TargetPassConfig *SLOW32TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new SLOW32PassConfig(*this, PM);
}

bool SLOW32PassConfig::addInstSelector() {
  addPass(createSLOW32ISelDag(getSLOW32TargetMachine()));
  return false;
}

void SLOW32PassConfig::addPreRegAlloc() {
  addPass(createSLOW32LoadAddrOptPass());
  TargetPassConfig::addPreRegAlloc();
}

MachineFunctionInfo *SLOW32TargetMachine::createMachineFunctionInfo(
    BumpPtrAllocator &Allocator, const Function &F,
    const TargetSubtargetInfo *STI) const {
  return SLOW32MachineFunctionInfo::create<SLOW32MachineFunctionInfo>(
      Allocator, F, STI);
}
