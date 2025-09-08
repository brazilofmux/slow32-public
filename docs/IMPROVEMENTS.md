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
1. **Full 32-bit addresses with LUI/ADDI**
   - Current: Only using ORI (16-bit immediates)
   - Need: `lui rd, %hi(symbol); addi rd, rd, %lo(symbol)`
   - Note: Must use ADDI not ORI for correct sign extension

2. **Switch statement control flow**
   - Currently generates incorrect branch labels
   - Jump table support needed

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

## Priority Order

1. **Runtime intrinsics** - Fix `bne` loops (causes infinite loops)
2. **Compiler LUI/ORI** - Switch to LUI/ADDI (causes incorrect addresses)
3. **Linker error handling** - Add hard errors for unresolved symbols
4. **Assembler `.word symbol`** - Support REL_32 relocations
5. **Emulator safety** - Improve shared loader checks

## Testing Recommendations

After implementing fixes:
1. Test varargs/printf functionality
2. Test multi-file linking with global symbols
3. Test negative offsets in data access
4. Test loops with complex control flow
5. Test all intrinsics with edge cases (0 size, odd sizes, etc.)