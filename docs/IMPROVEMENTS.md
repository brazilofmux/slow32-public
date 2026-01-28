# SLOW-32 Improvement Suggestions

This document consolidates feedback and improvement suggestions for the SLOW-32 toolchain components.

## Assembler Improvements

### Key Fixes Needed
- **Register names**: Make case-insensitive
- **`.word symbol`**: Support via REL_32 relocation for jump tables/data pointers
- **Auto-align in `.text`**: Use `bump_size()` for uniform section accounting
- **S-format parsing**: Explicit `rs2` handling
- **`%lo(symbol)`**: Keep in loads/stores exactly as-is

*(Note: Case handling and .string directive behavior have been verified as correct/fixed)*

## Toolchain Status (2026-01-27)

### Resolved / Verified
1. **64-bit Division Bug** ✅
   - The `UMUL_LOHI` custom lowering logic was verified to produce correct results.
   - `test_i64_div.c` passes with correct output.
   - Documentation in `docs/issues/llvm-i64-division.md` updated.

2. **Linker Error Handling** ✅
   - Linker now hard-errors on unresolved symbols (Exit Code 1).
   - Prevents generation of broken binaries with missing symbols.

3. **REL_CALL Implementation** ✅
   - `REL_CALL` relocation type is implemented in the linker (`s32-ld.c`).

4. **Linker Range Checks** ✅
   - LO12 "unpaired" check uses signed ±2048.
   - BRANCH upper bound tightened to +4094.
   - JAL upper bound tightened to +1,048,574.

5. **Printf Formatting** ✅
   - `printf` with `%llu` and other formats verified working correctly.

6. **Runtime Safety & Optimization** ✅
   - `memset` now uses `beq`-only loops, avoiding `bne` as per safety guidelines.
   - `malloc` upgraded to O(1) Segregated Free List allocator.
   - Floating-point code removed (unsupported); Integer math enabled.

7. **Emulator Safety** ✅
   - W^X protection logic unified across `slow32`, `slow32-fast`, and `slow32-dbt`.
   - Protection now respects `S32X_FLAG_W_XOR_X` header flag.
   - All three emulators use shared `s32x_loader.h` for executable loading.

8. **Shared Loader** ✅
   - All emulators (slow32, slow32-fast, dbt) now use callback-based `s32x_loader.h`.
   - `load_s32x_header()` for metadata, `load_s32x_file()` with write callback for sections.

### Open Issues
1. **Switch statement optimization**
   - Jump tables supported in assembler and linker
   - .word directive supports symbols via REL_32 relocations
   - LLVM backend may need tuning to avoid excessive .word generation

2. **Address formation correctness**
   - GEP scaling issues
   - Issues that cause "works in one TU, breaks when linked" bugs

## Emulator Improvements

### All Emulators (slow32, slow32-fast, slow32-dbt)
- **Shared loader**: All use `s32x_loader.h` with callback-based section loading ✅
- **Exit code**: All propagate guest exit code ✅
- **W^X protection**: Consistent enforcement from .s32x header flag ✅
- **MMIO**: Auto-detected from executable header ✅

## Priority Order (Updated 2026-01-27)

Most critical issues have been resolved:
- ✅ **64-bit Division** - Verified working.
- ✅ **Linker error handling** - Hard errors implemented.
- ✅ **Compiler LUI/ADDI** - Implemented for 32-bit addresses.
- ✅ **Linker library paths** - -L and -l flags implemented.
- ✅ **Shared loader** - All emulators unified.

Remaining lower-priority items:
1. **Switch statement optimization** - Jump tables tuning (LLVM backend).
2. **Address formation correctness** - GEP scaling issues (LLVM backend).

## Testing Recommendations

1. Test multi-file linking with global symbols (Verified)
2. Test negative offsets in data access
3. Test loops with complex control flow
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.)
5. Add string function regression tests (strlen, strcpy, memcpy, etc.)
6. Add stdlib regression tests (qsort, strtol, realloc)
