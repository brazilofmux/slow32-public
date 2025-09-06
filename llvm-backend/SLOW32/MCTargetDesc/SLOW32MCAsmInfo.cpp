//===-- SLOW32MCAsmInfo.cpp - SLOW32 Asm Info ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SLOW32MCAsmInfo.h"

using namespace llvm;

SLOW32MCAsmInfo::SLOW32MCAsmInfo(const Triple &TT) {
  CodePointerSize = CalleeSaveStackSlotSize = 4;
  PrivateGlobalPrefix = ".L";
  CommentString = "#";
  AlignmentIsInBytes = false;
  SupportsDebugInformation = true;
  ExceptionsType = ExceptionHandling::None;
  UseIntegratedAssembler = true;
  
  // SLOW32 assembler uses .word instead of .long for 32-bit data
  Data32bitsDirective = "\t.word\t";
}