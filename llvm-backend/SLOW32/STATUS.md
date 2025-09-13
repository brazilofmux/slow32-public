# SLOW32 Backend Status

Last Updated: 2025-09-03

## Overview
The SLOW32 LLVM backend is functional for basic C programs. It can compile functions with arguments, return values, arithmetic operations, memory access, global variables, and basic varargs support.

## Architecture Reminder
- 32-bit RISC architecture
- 32 registers (r0-r31)
  - r0: zero (hardwired to 0)
  - r1: return value
  - r2: temporary
  - r3-r10: argument registers (a0-a7)
  - r11-r28: saved registers (s0-s17)
  - r29: stack pointer (sp)
  - r30: frame pointer (fp)
  - r31: link register (lr)
- Fixed 32-bit instructions
- Simple addressing: base + 12-bit signed offset only
- Stack grows down from 0x0FFFFFF0

## ✅ Working Features

### Core Infrastructure
- Complete backend structure with all required TableGen files
- Register definitions and register classes
- Instruction formats (R-type, I-type, U-type, B-type, J-type)
- Basic SelectionDAG lowering
- Machine function pass pipeline

### Instructions
- **Arithmetic**: ADD, SUB, MUL, DIV, REM (+ immediate variants)
- **Logical**: AND, OR, XOR (+ immediate variants)
- **Shifts**: SLL, SRL, SRA (both immediate and register forms)
- **Memory**: 
  - Word: LDW, STW with base+offset addressing
  - Byte: LDB (sign-extend), LDBU (zero-extend), STB
  - Halfword: LDH (sign-extend), LDHU (zero-extend), STH
- **Comparison**: SLT, SLTU, SEQ, SNE, SGT, SGTU, SGE, SGEU
- **Control Flow**: JAL, JALR for calls; BEQ, BNE, BLT, BGE for branches
- **Constants**: LI (pseudo), LUI, ORI for loading immediates

### Function Support
- Function prologue/epilogue generation
- Saving/restoring fp and lr
- Stack frame allocation
- Register argument passing (r3-r10)
- Stack argument passing (>8 args)
- Return value in r1
- Proper handling of void returns (R1 defined as undef)

### Advanced Features
- **Global Variables**: %hi/%lo addressing pattern for 32-bit addresses
- **Function Pointers**: Direct calls via JAL/JALR
- **Basic Varargs**: 
  - va_start implementation
  - Saving unused arg registers
  - Simple variadic functions work

## ⚠️ Partially Working

### %hi/%lo Addressing
- Generates correct LUI + ORI sequences
- Relocations work at binary level
- ✅ Assembly syntax now properly emits %hi() and %lo() markers
- Distinguishes between function addresses (direct) and data addresses (%hi/%lo)

### Varargs
- Simple cases compile and work
- va_start properly implemented
- va_arg/va_end expanded by LLVM
- Complex cases with many arguments fail with register allocation errors
- Stack varargs not fully tested

### Switch Statements
- Basic lowering works
- Needs proper analyzeBranch implementation for optimization
- Jump tables not yet implemented

## ❌ Not Implemented

### Missing Branch Instructions
- BLE, BGT (signed comparisons)
- BLTU, BGEU, BLEU, BGTU (unsigned comparisons)
- These can be synthesized from existing branches

### CFG Optimization
- analyzeBranch not implemented (returns false)
- insertBranch stub implementation
- removeBranch stub implementation
- This limits LLVM's ability to optimize control flow

### Other Features
- Inline assembly support
- Thread-local storage
- Exception handling (may not be needed)
- Debug information
- Atomic operations (not in SLOW32 ISA)
- Floating point (not in SLOW32 ISA)

## 🐛 Known Issues

1. **Function Call Arguments**: ✅ FIXED - Arguments now being passed correctly
   - Fixed by using glue chains to connect CopyToReg nodes to CALL
   - Added variable_ops to JAL_CALL to handle dynamic implicit uses
   - Only adds implicit uses for actually-used argument registers
2. **Varargs**: ⚠️ PARTIALLY FIXED - Register save order corrected
   - Fixed reverse order saving of unused argument registers
   - Still requires clang changes for proper SLOW32 va_list structure
   - Currently using x86 __va_list_tag which is incompatible
3. **Memcpy Expansion**: Small constant memcpy generates incorrect code
   - LLVM expands small memcpy to load/store sequences
   - Generated code loads wrong bytes when building 32-bit values
   - Affects string constant initialization on stack
4. **Machine Code Verification**: -O0 compilation triggers verification failures
   - Missing barrier instructions after conditional branches
   - Undefined physical registers in some contexts
5. **Constant Folding**: Incorrect handling of (constant + register) in truncated contexts
   - Example: `(48 + i8_value)` generates wrong immediate (805306368 instead of 48)
6. **Branch Inversions**: Some condition inversions may be incorrect

## 📊 Regression Test Results (as of 2025-09-02, late night)

### Test Suite Status
- **Total Tests**: 13
- **Passing**: 5 
- **Failing**: 3
- **Skipped**: 5 (tests not yet written)

### ✅ Passing Tests
- `feature-arithmetic`: Basic arithmetic operations
- `feature-branches`: Branch operations  
- `feature-function-calls`: Function calls with arguments
- `feature-logical`: Logical operations (AND, OR, XOR)
- `feature-shifts`: Shift operations (SLL, SRL, SRA)

### ❌ Failing Tests
- `bug001-varargs-clobbered`: Varargs handling issue
- `bug005-getelementptr`: Array/pointer access issue
- `feature-loops`: Loop optimization/constant folding issue

### 📝 Skipped Tests
- `bug002`, `bug003`, `bug004`: Tests not yet created
- `feature-globals`, `feature-memory`: Tests not yet created

## 🎯 Next Steps (Priority Order)

1. **Fix Remaining Test Failures**
   - bug001-varargs-clobbered: Debug varargs handling
   - bug005-getelementptr: Fix array/pointer arithmetic
   - feature-loops: Investigate constant folding issue

2. **Fix -O0 Machine Code Verification**
   - Add proper barrier instructions after branches
   - Fix undefined physical register issues
   - Resolve constant folding problems in truncated contexts

3. **Complete Branch Support**
   - Implement analyzeBranch/insertBranch/removeBranch
   - Fix branch condition inversions
   - Add missing branch instructions (BLE, BGT, unsigned variants)

4. **Inline Assembly**
   - Basic constraint support
   - Register operand handling

5. **Testing & Validation**
   - Create comprehensive test suite
   - Test with real programs (maybe a simple OS kernel?)
   - Performance benchmarking

## 💡 Design Decisions

- **Following RISC-V closely**: RISC-V is a clean, modern RISC design without historical baggage
- **Minimal custom lowering**: Let LLVM handle complex operations via expansion
- **Correctness over performance**: Get it working first, optimize later
- **Simple ABI**: Fixed calling convention, no variability

## 📚 Resources

- SLOW32 ISA documentation: `~/slow-32/docs/`
- SLOW32 assembler: `~/slow-32/assembler/`
- SLOW32 emulator: `~/slow-32/emulator/`
- Backup script: `~/slow-32/scripts/backup-llvm-backend.sh`
- Restore script: `~/slow-32/scripts/restore-llvm-backend.sh`

## 🚀 How to Test

```bash
# Compile C to LLVM IR
clang -S -emit-llvm -O0 test.c -o test.ll

# Lower to SLOW32 assembly  
build/bin/llc -mtriple=slow32-unknown-none test.ll -o test.s

# Assemble
~/slow-32/assembler/slow32asm test.s -o test.o

# Link
~/slow-32/linker/slow32ld test.o -o test.exe

# Run
~/slow-32/emulator/slow32emu test.exe
```

## Recent Improvements (September 2025)

### Critical Optimizer Bug Fix - Function Arguments (Sept 8, 2025)
- **Problem**: Machine CSE optimizer was eliminating argument register setup at -O1 and above
- **Root Cause**: JAL_CALL/JALR_CALL instructions didn't mark argument registers (R3-R10) as implicit uses
- **Solution**: Modified SLOW32ISelDAGToDAG.cpp to walk glue chain and explicitly add used argument registers as operands
- **Impact**: ALL function calls with arguments now work correctly at all optimization levels
- **Files Modified**: `SLOW32ISelDAGToDAG.cpp`

### Critical Calling Convention Fix (Sept 3)
- **Fixed call instruction clobber lists** - JAL_CALL and JALR_CALL now correctly specify that they clobber all caller-saved registers (R1-R10, R31)
- **Impact**: Fixes register corruption bugs where LLVM would incorrectly keep live values in R2-R10 across function calls
- **Result**: Integer-to-hex conversion and other multi-call functions now work correctly

### Major Backend Enhancements (Sept 3)
- **Added Comprehensive Subtarget Features** - Defined instruction set features (I, M, F, A, C extensions) and performance features
- **Implemented Multiple Processor Models** - Added generic, minimal, v1, v2, embedded, and fp processor variants
- **Enhanced Scheduling Infrastructure** - Added detailed SchedWrite/SchedRead classes with accurate latencies for all instruction types
- **Explicit Instruction Properties** - Disabled instruction property guessing; all instructions now explicitly specify mayLoad, mayStore, hasSideEffects
- **Assembly Infrastructure** - Added AsmParser and AsmWriter with subtarget support
- **Fixed Pattern Warnings** - Removed redundant commutative patterns (LLVM canonicalizes immediates to RHS)

### Files Added/Modified
- Created `SLOW32Features.td` - Subtarget feature definitions
- Created `SLOW32Processors.td` - Processor model definitions  
- Enhanced `SLOW32Schedule.td` - Added comprehensive scheduling model
- Updated `SLOW32.td` - Integrated new features and proper include ordering
- Modified `SLOW32Subtarget.h/cpp` - Added feature support and getters
- Fixed `SLOW32InstrInfo.td` - Explicit instruction properties, removed redundant patterns
- Updated `SLOW32InstPrinter.h/cpp` - Added MCSubtargetInfo parameter support

### Testing Status
All 8 regression tests with test files passing:
- ✓ bug001-varargs-clobbered
- ✓ bug005-getelementptr
- ✓ feature-arithmetic
- ✓ feature-branches
- ✓ feature-function-calls
- ✓ feature-logical
- ✓ feature-loops
- ✓ feature-shifts

---
*This backend is under active development. Expect rough edges but steady progress.*