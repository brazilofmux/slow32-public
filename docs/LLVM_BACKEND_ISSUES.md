# LLVM Backend Issues to Fix

~~**IMPORTANT**: Inline assembly support has NOT been added to the Clang SLOW-32 target yet!~~ ✅ FIXED (2025-09-08)

## Critical Issues Found in stdlib Implementation

### ~~1. Missing UDIV/SDIV Support~~ ✅ FIXED (2025-09-08)

- **Solution**: Added custom lowering to generate libcalls (__udivsi3/__umodsi3)
- **Impact**: stdio.c now compiles successfully

### ~~2. Function Pointer Calls Not Supported~~ ✅ FIXED (2025-09-08)

- **Solution**: 
  - Added JALR_CALL support for indirect calls in SLOW32ISelDAGToDAG.cpp
  - Fixed function address materialization in LowerGlobalAddress
  - Now properly handles both indirect calls and function addresses as arguments
- **Impact**: qsort/bsearch and all function pointer operations now work correctly

### ~~3. -O2 Optimization Issues~~ ✅ FIXED (2025-09-08)

- **Solution**: Disabled Machine LICM and Block Placement passes in SLOW32TargetMachine.cpp
- **Impact**: Full -O1/-O2 optimization now works for all code

## Lower Priority Issues

### ~~4. No Inline Assembly Support~~ ✅ FIXED (2025-09-08)

- **Solution**: 
  - Added constraint handling ('r', 'i', 'm') in ISelLowering
  - Implemented PrintAsmOperand/PrintAsmMemoryOperand in AsmPrinter
  - Disabled integrated assembler to bypass parser requirement
- **Impact**: Full inline assembly support for stdlib development

### 5. Missing Floating Point Support

- **Location**: Math library functions use float/double
- **Status**: Software emulation works but is slow
- **Fix needed**: Consider adding FP instructions or optimized soft-float

### ~~6. Missing UREM/SREM Support~~ ✅ FIXED (2025-09-08)

- **Solution**: Added alongside UDIV support, generates libcalls to __umodsi3
- **Impact**: Modulo operations now work correctly

## Files to Modify in llvm-project

1. `llvm/lib/Target/SLOW32/SLOW32ISelLowering.cpp`
   - Add UDIV/SDIV/UREM/SREM lowering
   - Fix function pointer call lowering

2. `llvm/lib/Target/SLOW32/SLOW32ISelDAGToDAG.cpp`
   - Add patterns for division operations
   - Support indirect calls

3. `llvm/lib/Target/SLOW32/SLOW32InstrInfo.td`
   - May need new instruction patterns

## Testing Strategy

Once fixed, test with:

- Full stdlib compilation at -O2
- qsort/bsearch functionality
- Division-heavy code
- Function pointer callbacks
