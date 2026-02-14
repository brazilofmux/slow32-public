# SLOW-32 Runtime Library Map

## Core Assembly Files (Hand-written, must stay as .s)

- `crt0.s` - Startup code, provides _start entry point
- `divsi3.s` - Hand-optimized division routines (__udivsi3, __divsi3)
- `intrinsics.s` - LLVM intrinsics (memcpy, memset, lifetime markers)
- `intrinsics_extra.s` - Additional runtime support (memcpy alias, strlen)
- `stdlib.s` - putint function for integer output

## C Source Files (Compiled to .s then .s32o)

- `builtins.c` - 64-bit arithmetic builtins (__udivdi3, __divdi3, etc.)
- `convert_extra.c` - String conversion functions (itoa, utoa, ltoa, ultoa)
- `ctype.c` - Character classification (isalpha, isdigit, etc.)
- `debug_char.c` - DEBUG instruction wrapper (inline assembly)
- `flush_debug.c` - Flush function for DEBUG I/O backend
- `flush_mmio.c` - Flush function for MMIO I/O backend
- `malloc.c` - Memory allocation (malloc, free, calloc, realloc)
- `memory_extra.c` - Additional memory functions (memrchr)
- `printf.c` - Printf family implementation (CURRENTLY CAUSES INFINITE LOOP)
- `putchar.c` - Character output via DEBUG instruction (inline assembly)
- `stdio.c` - Standard I/O functions (fopen, fclose, etc.)
- `stdio_buffered.c` - Buffered I/O implementation
- `stdio_minimal.c` - Minimal stdio (puts function)
- `stdlib_extra.c` - Additional stdlib functions (qsort, bsearch, strtol)
- `stdlib_utils.c` - Utility functions (exit, abort, rand, srand)
- `string.c` - Basic string functions (strcpy, strcmp, strlen, etc.)
- `string_extra.c` - Additional string functions (strdup, strtok, etc.)
- `yield.c` - YIELD and HALT instructions (inline assembly)

## Archives Built

- `libs32.s32a` - Compiler runtime (intrinsics, division, builtins)
  - Contains: intrinsics.s32o, intrinsics_extra.s32o, builtins.s32o, 

              divsi3.s32o, debug_char.s32o, yield.s32o

- `libc_debug.s32a` - C library using DEBUG instruction for I/O
  - Contains: All string/memory/stdlib objects + putchar.s32o + stdio_minimal.s32o

- `libc_mmio.s32a` - C library using MMIO for buffered I/O  
  - Contains: All string/memory/stdlib objects + stdio.s32o + stdio_buffered.s32o + flush_mmio.s32o

## Link Order
```
crt0.s32o user.s32o libc_debug.s32a libs32.s32a
```
or
```
crt0.s32o user.s32o libc_mmio.s32a libs32.s32a
```

## Known Issues

- printf() enters infinite loop - needs debugging/rewrite
