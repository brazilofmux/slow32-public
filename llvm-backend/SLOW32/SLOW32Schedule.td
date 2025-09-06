//===-- SLOW32Schedule.td - SLOW32 Scheduling Definitions -----*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the scheduling model for SLOW32 processors.
// It includes both itinerary-based and SchedMachineModel-based scheduling.
//
//===----------------------------------------------------------------------===//

// Define instruction itinerary classes first
def IIC_ALU    : InstrItinClass;
def IIC_ALUi   : InstrItinClass;
def IIC_Load   : InstrItinClass;
def IIC_Store  : InstrItinClass;
def IIC_Branch : InstrItinClass;
def IIC_Jump   : InstrItinClass;
def IIC_Mul    : InstrItinClass;
def IIC_Div    : InstrItinClass;

// Basic functional units
def ALU : FuncUnit;
def MEM : FuncUnit;
def BRU : FuncUnit;  // Branch unit (BR conflicts with instruction name)

// Instruction Itineraries
def SLOW32Itineraries : ProcessorItineraries<
  [ALU, MEM, BRU], // Functional units
  [], // Bypass patterns  
  [
    // ALU operations - 1 cycle
    InstrItinData<IIC_ALU,    [InstrStage<1, [ALU]>]>,
    InstrItinData<IIC_ALUi,   [InstrStage<1, [ALU]>]>,
    
    // Memory operations - 1 cycle (SLOW32 has no cache delays)
    InstrItinData<IIC_Load,   [InstrStage<1, [MEM]>]>,
    InstrItinData<IIC_Store,  [InstrStage<1, [MEM]>]>,
    
    // Branch operations - 1 cycle
    InstrItinData<IIC_Branch, [InstrStage<1, [BRU]>]>,
    InstrItinData<IIC_Jump,   [InstrStage<1, [BRU]>]>,
    
    // Multiply/Divide - slightly more expensive
    InstrItinData<IIC_Mul,    [InstrStage<2, [ALU]>]>,
    InstrItinData<IIC_Div,    [InstrStage<4, [ALU]>]>
  ]>;

//===----------------------------------------------------------------------===//
// SchedWrite and SchedRead definitions for modern scheduling
//===----------------------------------------------------------------------===//

// Basic ALU operations
def WriteIALU       : SchedWrite; // Integer ALU operations
def WriteIALU32     : SchedWrite; // 32-bit specific ALU ops
def WriteShift      : SchedWrite; // Shift operations
def WriteShift32    : SchedWrite; // 32-bit shift operations

// Multiply/Divide
def WriteIMul       : SchedWrite; // Integer multiplication
def WriteIMul32     : SchedWrite; // 32-bit multiplication
def WriteIDiv       : SchedWrite; // Integer division
def WriteIDiv32     : SchedWrite; // 32-bit division
def WriteIRem       : SchedWrite; // Integer remainder
def WriteIRem32     : SchedWrite; // 32-bit remainder

// Memory operations
def WriteLDW        : SchedWrite; // Load word
def WriteLDH        : SchedWrite; // Load halfword
def WriteLDB        : SchedWrite; // Load byte
def WriteSTW        : SchedWrite; // Store word
def WriteSTH        : SchedWrite; // Store halfword
def WriteSTB        : SchedWrite; // Store byte

// Control flow
def WriteJump       : SchedWrite; // Unconditional jump
def WriteJAL        : SchedWrite; // Jump and link
def WriteJALR       : SchedWrite; // Jump and link register
def WriteBranch     : SchedWrite; // Conditional branch
def WriteCall       : SchedWrite; // Function call
def WriteRet        : SchedWrite; // Function return

// Special operations
def WriteNop        : SchedWrite; // No operation
def WriteLUI        : SchedWrite; // Load upper immediate
def WriteMove       : SchedWrite; // Register move
def WriteBarrier    : SchedWrite; // Memory barrier

// Read timings for operands
def ReadALU         : SchedRead; // ALU operand read
def ReadShift       : SchedRead; // Shift amount read
def ReadMul         : SchedRead; // Multiply operand read
def ReadDiv         : SchedRead; // Divide operand read
def ReadMemBase     : SchedRead; // Memory base address read
def ReadMemOffset   : SchedRead; // Memory offset read
def ReadStoreData   : SchedRead; // Store data operand read
def ReadBranch      : SchedRead; // Branch condition read
def ReadJump        : SchedRead; // Jump target read

//===----------------------------------------------------------------------===//
// SchedMachineModel for basic SLOW32
//===----------------------------------------------------------------------===//

def SLOW32Model : SchedMachineModel {
  let IssueWidth = 1;          // Single issue processor
  let MicroOpBufferSize = 0;   // Not superscalar
  let LoadLatency = 2;          // Loads take 2 cycles
  let MispredictPenalty = 3;    // Branch mispredict penalty
  let CompleteModel = 0;        // Model is not complete yet
}

// Define processor resources
def SLOW32_ALU  : ProcResource<1>; // Single ALU unit
def SLOW32_MEM  : ProcResource<1>; // Single memory unit
def SLOW32_BR   : ProcResource<1>; // Single branch unit
def SLOW32_MUL  : ProcResource<1>; // Multiply unit (may share with ALU)
def SLOW32_DIV  : ProcResource<1>; // Divide unit (may share with ALU)

// Map SchedWrites to processor resources and latencies
let SchedModel = SLOW32Model in {
  // ALU operations - 1 cycle
  def : WriteRes<WriteIALU, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteIALU32, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteShift, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteShift32, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteLUI, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteMove, [SLOW32_ALU]> { let Latency = 1; }
  
  // Multiply - 3 cycles
  def : WriteRes<WriteIMul, [SLOW32_MUL]> { let Latency = 3; }
  def : WriteRes<WriteIMul32, [SLOW32_MUL]> { let Latency = 3; }
  
  // Divide - 10 cycles (slow on simple processors)
  def : WriteRes<WriteIDiv, [SLOW32_DIV]> { let Latency = 10; }
  def : WriteRes<WriteIDiv32, [SLOW32_DIV]> { let Latency = 10; }
  def : WriteRes<WriteIRem, [SLOW32_DIV]> { let Latency = 10; }
  def : WriteRes<WriteIRem32, [SLOW32_DIV]> { let Latency = 10; }
  
  // Memory operations - 2 cycles
  def : WriteRes<WriteLDW, [SLOW32_MEM]> { let Latency = 2; }
  def : WriteRes<WriteLDH, [SLOW32_MEM]> { let Latency = 2; }
  def : WriteRes<WriteLDB, [SLOW32_MEM]> { let Latency = 2; }
  def : WriteRes<WriteSTW, [SLOW32_MEM]> { let Latency = 1; }
  def : WriteRes<WriteSTH, [SLOW32_MEM]> { let Latency = 1; }
  def : WriteRes<WriteSTB, [SLOW32_MEM]> { let Latency = 1; }
  
  // Control flow - 1 cycle, but may have mispredict penalty
  def : WriteRes<WriteJump, [SLOW32_BR]> { let Latency = 1; }
  def : WriteRes<WriteJAL, [SLOW32_BR]> { let Latency = 1; }
  def : WriteRes<WriteJALR, [SLOW32_BR]> { let Latency = 1; }
  def : WriteRes<WriteBranch, [SLOW32_BR]> { let Latency = 1; }
  def : WriteRes<WriteCall, [SLOW32_BR]> { let Latency = 1; }
  def : WriteRes<WriteRet, [SLOW32_BR]> { let Latency = 1; }
  
  // Special
  def : WriteRes<WriteNop, []> { let Latency = 1; }
  def : WriteRes<WriteBarrier, [SLOW32_MEM]> { let Latency = 1; }
  
  // Read timings - most are ready immediately
  def : ReadAdvance<ReadALU, 0>;
  def : ReadAdvance<ReadShift, 0>;
  def : ReadAdvance<ReadMul, 0>;
  def : ReadAdvance<ReadDiv, 0>;
  def : ReadAdvance<ReadMemBase, 0>;
  def : ReadAdvance<ReadMemOffset, 0>;
  def : ReadAdvance<ReadStoreData, 0>;
  def : ReadAdvance<ReadBranch, 0>;
  def : ReadAdvance<ReadJump, 0>;
}

//===----------------------------------------------------------------------===//
// SchedMachineModel for performance-optimized SLOW32
//===----------------------------------------------------------------------===//

def SLOW32FastModel : SchedMachineModel {
  let IssueWidth = 2;          // Can issue 2 instructions
  let MicroOpBufferSize = 4;   // Small reorder buffer
  let LoadLatency = 1;          // Fast memory
  let MispredictPenalty = 2;    // Better branch predictor
  let CompleteModel = 0;
}

// Fast model resource mappings (lower latencies)
let SchedModel = SLOW32FastModel in {
  // ALU operations - 1 cycle
  def : WriteRes<WriteIALU, [SLOW32_ALU]> { let Latency = 1; }
  def : WriteRes<WriteIMul, [SLOW32_MUL]> { let Latency = 2; } // Faster multiply
  def : WriteRes<WriteIDiv, [SLOW32_DIV]> { let Latency = 6; } // Faster divide
  def : WriteRes<WriteLDW, [SLOW32_MEM]> { let Latency = 1; } // Fast memory
}