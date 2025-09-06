//===-- SLOW32MCTargetDesc.h - SLOW32 Target Descriptions -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCTARGETDESC_H
#define LLVM_LIB_TARGET_SLOW32_MCTARGETDESC_SLOW32MCTARGETDESC_H

#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class Target;
class Triple;
class StringRef;

MCCodeEmitter *createSLOW32MCCodeEmitter(const MCInstrInfo &MCII,
                                          MCContext &Ctx);

MCAsmBackend *createSLOW32AsmBackend(const Target &T,
                                      const MCSubtargetInfo &STI,
                                      const MCRegisterInfo &MRI,
                                      const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter> createSLOW32ELFObjectWriter(uint8_t OSABI);

} // end namespace llvm

// Defines symbolic names for SLOW32 registers
#define GET_REGINFO_ENUM
#include "SLOW32GenRegisterInfo.inc"

// Defines symbolic names for SLOW32 instructions
#define GET_INSTRINFO_ENUM
#include "SLOW32GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "SLOW32GenSubtargetInfo.inc"

#endif