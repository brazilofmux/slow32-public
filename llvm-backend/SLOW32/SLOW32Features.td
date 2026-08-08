//===-- SLOW32Features.td - SLOW32 Features and Extensions ----*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// SLOW32 Subtarget features
//===----------------------------------------------------------------------===//

// Basic instruction set (always enabled)
def FeatureStdExtI : SubtargetFeature<"i", "HasStdExtI", "true",
                                      "'I' (Base Integer Instructions)">;

// Multiplication and division support
def FeatureStdExtM : SubtargetFeature<"m", "HasStdExtM", "true",
                                      "'M' (Integer Multiplication and Division)",
                                      [FeatureStdExtI]>;

// Soft-float in GPRs with native f32/f64 opcodes
def FeatureStdExtF : SubtargetFeature<"f", "HasStdExtF", "true",
                                      "'F' (Floating-Point in GPRs)",
                                      [FeatureStdExtI]>;

// Atomic operations support (unused: single-hart NotAtomic expansion)
def FeatureStdExtA : SubtargetFeature<"a", "HasStdExtA", "true",
                                      "'A' (Atomic Instructions)",
                                      [FeatureStdExtI]>;

// Compressed instructions (future, if SLOW32 adds 16-bit instructions)
def FeatureStdExtC : SubtargetFeature<"c", "HasStdExtC", "true",
                                      "'C' (Compressed Instructions)",
                                      [FeatureStdExtI]>;

//===----------------------------------------------------------------------===//
// Performance and optimization features
//===----------------------------------------------------------------------===//

// Fast multiplication (single-cycle)
def FeatureFastMul : SubtargetFeature<"fast-mul", "HasFastMul", "false",
                                      "Fast multiplication">;

// Fast division
def FeatureFastDiv : SubtargetFeature<"fast-div", "HasFastDiv", "false",
                                      "Fast division">;

// Misaligned memory access support
def FeatureMisalignedAccess : SubtargetFeature<"misaligned", "HasMisalignedAccess", "false",
                                               "Support misaligned memory accesses">;

// Branch predictor
def FeatureBranchPredictor : SubtargetFeature<"branch-predictor", "HasBranchPredictor", "false",
                                              "Has branch predictor">;

// Return address stack
def FeatureReturnStack : SubtargetFeature<"return-stack", "HasReturnStack", "false",
                                          "Has return address stack">;

//===----------------------------------------------------------------------===//
// Tuning features
//===----------------------------------------------------------------------===//

// Optimize for size
def TuneOptimizeSize : SubtargetFeature<"optimize-size", "OptimizeForSize", "false",
                                        "Optimize for code size">;

// Prefer immediate operands
def TunePreferImmediate : SubtargetFeature<"prefer-immediate", "PreferImmediate", "true",
                                           "Prefer immediate operands when possible">;

// Fuse shift-and-mask operations
def TuneFuseShiftMask : SubtargetFeature<"fuse-shift-mask", "FuseShiftMask", "false",
                                         "Fuse shift and mask operations">;

//===----------------------------------------------------------------------===//
// Feature groups for standard configurations  
// These are just lists, not TableGen defs - they're used in processor definitions
//===----------------------------------------------------------------------===//