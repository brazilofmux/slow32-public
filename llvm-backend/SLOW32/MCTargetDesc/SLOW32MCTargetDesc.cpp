//===-- SLOW32MCTargetDesc.cpp - SLOW32 Target Descriptions -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32MCTargetDesc.h"
#include "SLOW32InstPrinter.h"
#include "SLOW32MCAsmInfo.h"
#include "TargetInfo/SLOW32TargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "SLOW32GenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "SLOW32GenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "SLOW32GenSubtargetInfo.inc"

using namespace llvm;

static MCInstrInfo *createSLOW32MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitSLOW32MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createSLOW32MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitSLOW32MCRegisterInfo(X, SLOW32::R31); // Link register
  return X;
}

static MCSubtargetInfo *createSLOW32MCSubtargetInfo(const Triple &TT,
                                                     StringRef CPU,
                                                     StringRef FS) {
  return createSLOW32MCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCAsmInfo *createSLOW32MCAsmInfo(const MCRegisterInfo &MRI,
                                         const Triple &TT,
                                         const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new SLOW32MCAsmInfo(TT);
  return MAI;
}

static MCInstPrinter *createSLOW32InstPrinter(const Triple &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new SLOW32InstPrinter(MAI, MII, MRI);
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSLOW32TargetMC() {
  // Register the MC asm info.
  for (Target *T : {&getTheSLOW32Target()}) {
    TargetRegistry::RegisterMCAsmInfo(*T, createSLOW32MCAsmInfo);
    
    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createSLOW32MCInstrInfo);
    
    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createSLOW32MCRegisterInfo);
    
    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createSLOW32MCSubtargetInfo);
    
    // Register the MCInstPrinter
    TargetRegistry::RegisterMCInstPrinter(*T, createSLOW32InstPrinter);
    
    // Register the MCCodeEmitter
    TargetRegistry::RegisterMCCodeEmitter(*T, createSLOW32MCCodeEmitter);
    
    // Register the ASM Backend
    TargetRegistry::RegisterMCAsmBackend(*T, createSLOW32AsmBackend);
  }
}