//===-- SLOW32.td - Describe the SLOW32 Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This is the top-level TableGen file for the SLOW32 target.
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// SLOW32 subtarget features and processors
//===----------------------------------------------------------------------===//

include "SLOW32Features.td"

//===----------------------------------------------------------------------===//
// Register file, instruction descriptions
//===----------------------------------------------------------------------===//

include "SLOW32RegisterInfo.td"
include "SLOW32Schedule.td"

// Processor models must come after Schedule definitions
include "SLOW32Processors.td"
include "SLOW32InstrInfo.td"
include "SLOW32CallingConv.td"

//===----------------------------------------------------------------------===//
// SLOW32 InstrInfo
//===----------------------------------------------------------------------===//

def SLOW32InstrInfo : InstrInfo {
  // Do not guess instruction properties - be explicit
  let guessInstructionProperties = 0;
}

//===----------------------------------------------------------------------===//
// SLOW32 Assembly Parser
//===----------------------------------------------------------------------===//

def SLOW32AsmParser : AsmParser {
  // Allow alternative register name matching
  let ShouldEmitMatchRegisterAltName = 1;
  // Allow parsing of .variant_cc directives
  let HasMnemonicFirst = 0;
}

//===----------------------------------------------------------------------===//
// SLOW32 Assembly Writer  
//===----------------------------------------------------------------------===//

def SLOW32AsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  // Pass subtarget features to the asmwriter
  int PassSubtarget = 1;
  // Variant for printing
  int Variant = 0;
}

//===----------------------------------------------------------------------===//
// Top-level target definition
//===----------------------------------------------------------------------===//

def SLOW32 : Target {
  let InstructionSet = SLOW32InstrInfo;
  let AssemblyParsers = [SLOW32AsmParser];
  let AssemblyWriters = [SLOW32AsmWriter];
  // Enable register renaming for better register allocation
  let AllowRegisterRenaming = 1;
}