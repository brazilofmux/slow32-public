# SLOW-32 Improvement Suggestions

This document consolidates feedback and improvement suggestions for the SLOW-32 toolchain components.

## Assembler Improvements

### Key Fixes Needed
- **Case handling**: Lowercase only the mnemonic/directive, preserve symbol case
- **Register names**: Make case-insensitive
- **`.word symbol`**: Support via REL_32 relocation for jump tables/data pointers
- **`.string` directive**: Emit NUL after every argument, not just at the end
- **Auto-align in `.text`**: Use `bump_size()` for uniform section accounting
- **S-format parsing**: Explicit `rs2` handling
- **`%lo(symbol)`**: Keep in loads/stores exactly as-is

## Toolchain Status (2025-09-08)

### New Features
1. **Archive tool (s32-ar)** ✅
   - Create, list, extract .s32a archives
   - Symbol index for efficient linking
   - Compatible with standard ar operations
   
2. **Runtime library archive** ✅
   - All runtime components in libruntime.s32a
   - Includes expanded stdlib, ctype, string functions
   
3. **Expanded C library** ✅
   - qsort, bsearch, strtol, strtok_r
   - Full ctype.h implementation
   - div/ldiv functions
   - Memory and string utilities

### Recently Fixed (2025-09-09)
1. **~~CRITICAL: Function arguments not passed for external symbols~~** ✅ FIXED/FALSE ALARM
   - **2025-09-09 Investigation**: This was a misdiagnosis - LLVM backend DOES generate correct code!
   - Tested: `puts()`, `strcpy()`, and other external calls work perfectly
   - Arguments ARE properly loaded into registers (r3, r4, etc.) before JAL
   - Assembler DOES create proper relocations for external symbols
   - All test cases link and execute successfully
   
2. **Memory fault in malloc/printf** ✅ FIXED
   - Root cause was in malloc implementation
   - Test programs now run successfully
   - malloc/free tested with stress tests (100+ allocations)
   
3. **Linker library paths** ✅ IMPLEMENTED
   - -L flag now implemented for library search paths
   - -l flag for linking libraries (e.g., -lc links libc.s32a)
   - Supports multiple search paths

## Compiler Improvements

### Recently Fixed (2025-09-01)
1. **Support for >8 function arguments** ✅
   - Stack arguments now properly pushed/popped
   - Caller allocates space and stores args at SP+0, SP+4, etc.
   - Callee loads from FP+8, FP+12, etc.
   
2. **Global variable addressing** ✅
   - Fixed to use symbol names instead of raw hex addresses
   - Pattern: `ori rd, r0, symbol` then `ldw rd, rd+0`
   
3. **LI pseudo-instruction** ✅
   - Now expands to `ori rd, r0, imm` for assembler compatibility

### Still Needed
1. **~~Full 32-bit addresses with LUI/ADDI~~** ✅ IMPLEMENTED
   - LLVM backend now generates LUI/ADDI for 32-bit addresses
   - Properly uses `lui rd, %hi(symbol)` and `addi rd, rd, %lo(symbol)`
   - Tested and working with high memory addresses

2. **Switch statement control flow**
   - Jump tables supported in assembler and linker
   - .word directive supports symbols via REL_32 relocations
   - LLVM backend may need tuning to avoid excessive .word generation

3. **Address formation correctness**
   - GEP scaling issues
   - Issues that cause "works in one TU, breaks when linked" bugs

## Emulator Improvements

### Both Emulators (slow32 and slow32-fast)
- **Encoding/PC bases**: B uses PC+4, JAL uses PC (currently correct)
- **W^X protection**: Fast checks `rodata_limit`, slow checks `wxorx_enabled` 
- **Shared loader**: Needs safety improvements

## Linker Improvements

### Error Handling
- Hard-error if relocation's symbol can't be resolved (even with `-s`)
- Hard-error if failing to map relocation's section
- Correct **LO12** "unpaired" range check (use signed ±2048, not unsigned)
- Tighten **BRANCH** upper bound to **+4094** (even offsets only)
- Tighten **JAL** upper bound to **+1,048,574** (even offsets only)
- Implement **REL_CALL** as JAL-style PC-relative call

## Runtime/Intrinsics Issues

### Critical Bug
**Problem**: Loops in intrinsics use `bne` which can spin infinitely if emulator's `bne` handling is off

**Fix**: Replace all `bne` loops with `beq`-only form in:
- `memcpy`
- `memset` 
- Other runtime intrinsics

**Additional Guards**: Add checks for edge cases (size ≤ 0, not multiples of 4) to prevent spins

## Priority Order (Updated 2025-09-09)

Most critical issues have been resolved:
- ✅ **Runtime intrinsics** - Core intrinsics (memcpy/memset) use safe BEQ patterns
- ✅ **Compiler LUI/ADDI** - Implemented for 32-bit addresses
- ✅ **Assembler `.word symbol`** - REL_32 relocations working
- ✅ **Linker library paths** - -L and -l flags implemented

Remaining lower-priority items:
1. **Linker error handling** - Add hard errors for unresolved symbols
2. **Emulator safety** - Improve shared loader checks
3. **Printf formatting** - Output formatting needs improvement
4. **BNE usage in stdlib** - Some library functions still use BNE (but working)

## Testing Recommendations

After implementing fixes:
1. Test varargs/printf functionality
2. Test multi-file linking with global symbols
3. Test negative offsets in data access
4. Test loops with complex control flow
5. Test all intrinsics with edge cases (0 size, odd sizes, etc.)