//===-- SLOW32Processors.td - SLOW32 Processor Models ---------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// SLOW32 Processor Models
//===----------------------------------------------------------------------===//

// Base class for SLOW32 processor models
class SLOW32ProcessorModel<string n, SchedMachineModel m,
                           list<SubtargetFeature> f,
                           list<SubtargetFeature> tunef = []>
    : ProcessorModel<n, m, !listconcat(f, tunef)>;

// Generic SLOW32 processor - conservative scheduling
def : SLOW32ProcessorModel<"generic-slow32", SLOW32Model, 
                           [FeatureStdExtI, FeatureStdExtM, FeatureStdExtF]>;

// Minimal SLOW32 processor - no multiply/divide
def : SLOW32ProcessorModel<"slow32-minimal", SLOW32Model, 
                           [FeatureStdExtI],
                           [TuneOptimizeSize]>;

// SLOW32 v1.0 - Basic implementation with multiply/divide
def : SLOW32ProcessorModel<"slow32-v1", SLOW32Model, 
                           [FeatureStdExtI, FeatureStdExtM, FeatureStdExtF],
                           [TunePreferImmediate]>;

// SLOW32 v2.0 - Performance optimized
def : SLOW32ProcessorModel<"slow32-v2", SLOW32FastModel, 
                           [FeatureStdExtI, FeatureStdExtM, FeatureStdExtF,
                            FeatureFastMul, FeatureBranchPredictor,
                            FeatureReturnStack],
                           [TunePreferImmediate, TuneFuseShiftMask]>;

// Future SLOW32 with floating point
def : SLOW32ProcessorModel<"slow32-fp", SLOW32Model, 
                           [FeatureStdExtI, FeatureStdExtM, FeatureStdExtF],
                           [TunePreferImmediate]>;

// Embedded SLOW32 optimized for size
def : SLOW32ProcessorModel<"slow32-embedded", SLOW32Model, 
                           [FeatureStdExtI, FeatureStdExtM, FeatureStdExtF],
                           [TuneOptimizeSize, TunePreferImmediate]>;
