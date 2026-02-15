# C Language Subset for Self-Hosting

> **V2 cross-reference:** Defines the language scope for **Stage 4** (Subset C compiler) in [BOOTSTRAP-V2.md](BOOTSTRAP-V2.md). The Full C compiler (**Stage 11**) extends beyond this subset.

## Overview

The SLOW-32 toolchain comprises 5 tools totaling ~6,993 lines of C plus ~286 lines of shared headers. This document catalogs the exact C features and libc functions required by a bootstrap compiler.

## Source Inventory

| File | Lines | Purpose |
|------|------:|---------|
| `tools/assembler/slow32asm.c` | 2,573 | Assembler |
| `tools/linker/s32-ld.c` | 2,289 | Linker |
| `tools/utilities/s32-ar.c` | 672 | Archiver |
| `tools/utilities/slow32dis.c` | 659 | Disassembler |
| `tools/utilities/slow32dump.c` | 513 | Dump utility |
| `common/s32_formats.h` | 157 | Binary format definitions |
| `common/s32_hashtable.h` | 129 | Hash table (header-only) |
| **Total** | **6,992** | |

## C Language Features

### YES — Required

| Feature | Used By | Notes |
|---------|---------|-------|
| `int`, `char`, `void` | All | Basic types |
| `uint8_t`, `uint16_t`, `uint32_t` | All | Fixed-width via `<stdint.h>` |
| `int32_t`, `int16_t`, `int8_t` | All | Signed fixed-width |
| `size_t` | All | From `<stddef.h>` or `<stdlib.h>` |
| `long` | s32-ar, slow32dump | File sizes (`ftell` return type) |
| `bool`, `true`, `false` | All except s32-ar | From `<stdbool.h>` |
| `struct` | All | 40+ struct definitions |
| `enum` | All | 20+ enum definitions |
| `typedef` | All | Extensive use |
| Pointers | All | Single and double indirection |
| Pointer arithmetic | All | Array traversal patterns |
| Arrays (1D) | All | Static and dynamic |
| `for`, `while` loops | All | Standard iteration |
| `do-while` | Rare | Hashtable probing |
| `if`/`else` | All | Standard branching |
| `switch`/`case` | All | Instruction dispatch, option parsing |
| `break`, `continue` | All | Loop/switch control |
| `return` | All | Function return |
| `static` functions | All | File-scoped helpers |
| `static` local variables | slow32dis, slow32dump | Rotating buffers |
| `const` | All | Read-only parameters |
| Cast expressions | All | `(uint32_t)`, `(int32_t)`, `(char *)` |
| Ternary `? :` | All | Common in expressions |
| String literals | All | Error messages, format strings |
| Character constants | All | `'a'`, `'\n'`, `'\0'` |
| Designated initializers | s32-ar, slow32asm | `{.field = value}` |
| `sizeof` | All | Structure sizes, allocation |
| All arithmetic operators | All | `+`, `-`, `*`, `/`, `%` |
| All bitwise operators | All | `&`, `\|`, `^`, `~`, `<<`, `>>` |
| All comparison operators | All | `==`, `!=`, `<`, `>`, `<=`, `>=` |
| Logical operators | All | `&&`, `\|\|`, `!` |
| Compound assignment | All | `+=`, `-=`, `\|=`, `&=`, `<<=`, `>>=` |
| Increment/decrement | All | `++`, `--` (prefix and postfix) |
| `#include` | All | Both `"local"` and `<system>` |
| `#define` (object-like) | All | Constants, magic numbers |
| `#define` (function-like) | asm, linker | `DARR_PUSH` macro |
| `#ifdef`/`#ifndef` | Headers | Include guards |
| `#if`/`#else`/`#endif` | Headers | Conditional compilation |
| `#pragma pack` | s32_formats.h | Struct packing (can ignore for bootstrap) |
| Recursion | slow32asm | Expression parser |
| Structure member access | All | `.field` and `->field` |
| Array of structs | All | Section tables, symbol tables |
| Nested structs | Linker | `combined_section_t` contains members |

### NO — Not Required

| Feature | Notes |
|---------|-------|
| `float`, `double` | Zero floating-point in toolchain |
| `va_list`, `...` | No varargs |
| Function pointers | `qsort` comparator is the only edge case (see below) |
| `union` | Not used |
| `goto` | Not used |
| Bitfields | Bit masks used instead |
| VLAs | Not used |
| Compound literals | Not used |
| `long long` / 64-bit | Toolchain uses `uint32_t` exclusively |
| `volatile`, `register`, `restrict` | Not used |
| Variadic macros | Not used |
| Token pasting (`##`) | Not used |
| Stringification (`#`) | Not used |
| `_Generic`, `_Atomic` | Not used |
| Complex numbers | Not used |
| Flexible array members | Not used |

### Edge Cases

**`qsort` comparator:** `slow32dis.c` passes a comparison function to `qsort`. This is the only function pointer in the toolchain. Workaround: replace `qsort` with a built-in sort that doesn't need function pointers, or add minimal function-pointer support just for this case.

**`#pragma pack(push, 1)`:** Used in `s32_formats.h` and `mmio_ring_layout.h` for byte-aligned structures. On SLOW-32, the emulator handles misaligned access transparently. The bootstrap compiler can ignore `#pragma pack` — natural alignment will add padding but the binary format reading code uses `fread` into properly typed fields, so it works regardless.

**Designated initializers:** Used in a few places (`{.field = value}`). Can be rewritten to positional initialization if the compiler doesn't support them.

**`inline` functions:** `s32_hashtable.h` uses `static inline`. The bootstrap compiler can treat `inline` as a no-op (compile as regular static functions).

## libc Function Inventory

### Memory Management (4 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `malloc` | ~60 | All |
| `calloc` | ~25 | All |
| `realloc` | ~8 | asm, linker (dynamic arrays) |
| `free` | ~50 | All |

### String Operations (11 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `strlen` | ~100 | All |
| `strcmp` | ~50 | All |
| `strncmp` | ~20 | All |
| `strcpy` | ~15 | All |
| `strncpy` | ~10 | asm, linker |
| `snprintf` | ~20 | All |
| `strdup` | ~5 | s32-ar, slow32dis |
| `strchr` | ~3 | s32-ar |
| `strrchr` | ~3 | s32-ar |
| `memcpy` | ~20 | All |
| `memset` | ~5 | All |

### File I/O (9 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `fopen` | ~20 | All |
| `fclose` | ~15 | All |
| `fread` | ~30 | All |
| `fwrite` | ~15 | All |
| `fseek` | ~40 | All |
| `ftell` | ~10 | All |
| `fgetc` | ~3 | s32-ar |
| `fputc` | ~3 | s32-ar |
| `fileno` | ~2 | s32-ar, slow32dump |

### Formatted Output (3 functions)

| Function | Calls | Format Specs Used |
|----------|:-----:|-------------------|
| `printf` | ~80 | `%d`, `%u`, `%x`, `%08x`, `%s`, `%c` |
| `fprintf` | ~100 | Same as printf (stderr) |
| `perror` | ~3 | Error messages |

### Character Classification (6 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `isspace` | ~3 | slow32asm |
| `isdigit` | ~3 | slow32asm |
| `isalpha` | ~3 | slow32asm |
| `isalnum` | ~3 | slow32asm |
| `isprint` | ~2 | slow32dump |
| `tolower` | ~3 | slow32asm |

### Numeric Conversion (5 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `atoi` | ~2 | slow32asm |
| `strtol` | ~3 | slow32asm |
| `strtoul` | ~2 | slow32dis |
| `strtoll` | ~2 | slow32asm (large literals) |
| `strtoull` | ~2 | slow32asm |

### Other (5 functions)

| Function | Calls | Used By |
|----------|:-----:|---------|
| `qsort` | ~2 | slow32dis |
| `exit` | ~8 | All |
| `access` | ~1 | s32-ar (F_OK check) |
| `fstat` / `stat` | ~2 | s32-ar, slow32dump |
| `localtime` / `strftime` | ~1 | s32-ar (timestamp display) |

**Total: ~37 unique libc functions**, ~550 call sites.

### Functions That Can Be Simplified

- `access()` → replace with `fopen` + `fclose` (just checking file existence)
- `fstat()` → replace with `fseek(SEEK_END)` + `ftell` (just getting file size)
- `localtime()` / `strftime()` → stub out (only used for archive listing display)
- `fileno()` → only used with fstat, can be eliminated if fstat is replaced
- `qsort()` → replace with inline insertion sort (only sorts small symbol tables)

With these simplifications, the core requirement drops to ~30 libc functions.

## Preprocessor Requirements

| Feature | Required | Notes |
|---------|:--------:|-------|
| `#include "file.h"` | Yes | Relative path resolution needed |
| `#include <stdint.h>` | Yes | Must provide standard type headers |
| `#define NAME value` | Yes | Object-like macros |
| `#define NAME(a,b) expr` | Yes | Function-like macros (DARR_PUSH) |
| `#ifdef` / `#ifndef` | Yes | Include guards |
| `#if` / `#elif` / `#else` / `#endif` | Yes | Conditional compilation |
| `#undef` | Rare | Mostly avoidable |
| `#pragma pack` | Ignorable | Can treat as no-op |
| `#pragma once` | No | Not used |
| `__VA_ARGS__` | No | Not used |
| `##` (token paste) | No | Not used |
| `#` (stringify) | No | Not used |
| `defined()` in `#if` | Maybe | Can be avoided |

## What Could Be Simplified in Toolchain Sources

If the toolchain sources were modified slightly for bootstrap-friendliness:

1. **Replace `qsort` calls** with inline sort — eliminates the only function pointer
2. **Replace `snprintf`** with simpler formatting — reduces printf complexity
3. **Replace `strdup`** with `malloc` + `strcpy` — trivial
4. **Replace `strtoll`/`strtoull`** with `strtol`/`strtoul` — no 64-bit needed
5. **Replace designated initializers** with positional — simpler parser
6. **Remove `#pragma pack`** — not needed on SLOW-32 with emulated memory

These changes would be minimal and could be guarded with `#ifdef BOOTSTRAP_CC`.
