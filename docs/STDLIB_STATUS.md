# SLOW-32 Standard Library Status

## Overview
The SLOW-32 standard library is now largely functional, supporting both simple DEBUG-based I/O and high-performance MMIO. Previous LLVM backend limitations have been resolved.

## Completed Components

### String Library (string.h) - WORKING ✅

- `strlen`, `strcpy`, `strncpy`, `strcmp`, `strncmp`, `strcat`, `strncat`, `strchr`, `strrchr`, `strstr` - PASS
- `memset`, `memcpy`, `memmove`, `memcmp`, `memchr` - PASS
- `memmove` correctly handles overlapping buffers.

### Standard Library (stdlib.h) - WORKING ✅

- `malloc`, `free`, `calloc`, `realloc` - Fully functional with MMIO-based heap.
- `abs`, `labs`, `atoi`, `atol` - PASS
- `rand`, `srand` - PASS
- `exit`, `abort` - PASS (MMIO and DEBUG variants)
- `qsort`, `bsearch` - PASS (Function pointers now fully supported)
- `strtol`, `strtoul` - PASS
- `div`, `ldiv` - PASS (Division operations fully supported)

### Standard I/O (stdio.h) - WORKING ✅

- `printf`, `sprintf`, `snprintf`, `vsnprintf` - PASS (Full implementation with width/precision)
- `puts`, `putchar` - PASS
- `fopen`, `fclose`, `fread`, `fwrite` - PASS (MMIO-based file I/O)
- `stat`, `fstat` - PASS (MMIO-based file metadata)

### Math Library (math.h) - INTEGER ONLY ⚠️

- Integer math functions available in `math_int.c` (`iabs`, `imin`, `imax`, `isqrt`, `ipow`).
- **NO FLOATING POINT SUPPORT**: The LLVM backend crashes on float/double operations. All standard FP functions in `math.h` are commented out.
- Native 32-bit `mul`, `div`, `rem` instructions used for integer performance.

## Resolved LLVM Backend Issues

### Fixed (2025-09-08)

1. **UDIV/SDIV/UREM/SREM Support** ✅
   - Custom lowering added to generate libcalls to `__udivsi3`/`__umodsi3`.
   - Now supports all division and modulo operations in C.

2. **Function Pointer Calls** ✅
   - `JALR_CALL` support added for indirect calls.
   - Function addresses as arguments and callbacks (like in `qsort`) now work perfectly.

3. **Optimization Levels** ✅
   - Full `-O1` and `-O2` optimization supported for all code.
   - Fixed Machine LICM and Block Placement issues.

4. **Inline Assembly Support** ✅
   - Added constraint handling for `r`, `i`, `m` in Clang.
   - `asm("halt")` and other inline assembly instructions now work correctly.

5. **64-bit Arithmetic** ✅
   - Verified `__udivdi3` and other 64-bit builtins.
   - Hand-coded `UMUL_LOHI` lowering fixed to ensure correct carries.

## File Structure
```
runtime/
├── include/
│   ├── string.h     ✅ Complete
│   ├── stdlib.h     ✅ Complete  
│   ├── stdio.h      ✅ Complete
│   └── math.h       ✅ Complete
├── string.c         ✅ String functions
├── malloc.c         ✅ Dynamic memory allocation
├── stdlib_utils.c   ✅ Conversion and basic utils
├── stdlib_extra.c   ✅ qsort, bsearch, strtol
├── stdio.c          ✅ MMIO-based file operations
├── printf_enhanced.c ✅ Full-featured printf implementation
└── builtins.c       ✅ 64-bit arithmetic support
```

## Build and Link Order
The recommended link order is:
`s32-ld -o prog.s32x crt0.s32o user.s32o libc_mmio.s32a libs32.s32a`

Use `libc_debug.s32a` instead of `libc_mmio.s32a` for simple console output via the `DEBUG` instruction without MMIO infrastructure requirements.
