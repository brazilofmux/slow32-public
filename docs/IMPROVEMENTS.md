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

## Toolchain Status (2025-12-27)

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

4. **Printf Formatting** ✅
   - `printf` with `%llu` and other formats verified working correctly.

### Open Issues
1. **BNE usage in stdlib** ⚠️
   - Some runtime intrinsics (e.g., `memset` in `intrinsics.s`) still use `bne` loops.
   - Goal: Replace with `beq`-only forms for safer emulation.

2. **Emulator Safety**
   - Shared loader checks (`rodata_limit` vs `wxorx_enabled`) need unification/improvement.

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
1. **Switch statement control flow**
   - Jump tables supported in assembler and linker
   - .word directive supports symbols via REL_32 relocations
   - LLVM backend may need tuning to avoid excessive .word generation

2. **Address formation correctness**
   - GEP scaling issues
   - Issues that cause "works in one TU, breaks when linked" bugs

## Emulator Improvements

### Both Emulators (slow32 and slow32-fast)
- **Encoding/PC bases**: B uses PC+4, JAL uses PC (currently correct)
- **W^X protection**: Fast checks `rodata_limit`, slow checks `wxorx_enabled` 
- **Shared loader**: Needs safety improvements

## Linker Improvements

### Error Handling
- Hard-error if relocation's symbol can't be resolved (even with `-s`) ✅ DONE
- Hard-error if failing to map relocation's section
- Correct **LO12** "unpaired" range check (use signed ±2048, not unsigned)
- Tighten **BRANCH** upper bound to **+4094** (even offsets only)
- Tighten **JAL** upper bound to **+1,048,574** (even offsets only)
- Implement **REL_CALL** as JAL-style PC-relative call ✅ DONE

## Priority Order (Updated 2025-12-27)

Most critical issues have been resolved:
- ✅ **64-bit Division** - Verified working.
- ✅ **Linker error handling** - Hard errors implemented.
- ✅ **Compiler LUI/ADDI** - Implemented for 32-bit addresses.
- ✅ **Linker library paths** - -L and -l flags implemented.

Remaining lower-priority items:
1. **BNE usage in stdlib** - `memset` still uses BNE.
2. **Emulator safety** - Improve shared loader checks.
3. **Switch statement optimization** - Jump tables tuning.
4. **Assembler refinements** - Case handling, `.string` directive behavior.

## Testing Recommendations

1. Test multi-file linking with global symbols (Verified)
2. Test negative offsets in data access
3. Test loops with complex control flow
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.)
