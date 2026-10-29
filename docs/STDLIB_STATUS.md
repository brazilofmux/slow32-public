# SLOW-32 Standard Library Status

## Overview
The SLOW-32 standard library (`runtime/`) is the C library for clang-built
programs. It supports both simple DEBUG-based I/O and MMIO ring-buffer I/O.
The self-hosted toolchain has a second C library, `selfhost/stage08/libc/`;
some sources are built into both (printf, dtoa, `time_std.c`,
`strerror.c`), and `regression/run-libc-differential.sh` builds each of
its tests against both and, for tests marked HOSTLEG, against the host's
libc as well.

Open defects and their history are kept in `runtime/ISSUES.md`.

## Completed Components

### String Library (string.h, strings.h) - WORKING ✅

- `strlen`, `strcpy`, `strncpy`, `strcmp`, `strncmp`, `strcat`, `strncat`, `strchr`, `strrchr`, `strstr` - PASS
- `memset`, `memcpy`, `memmove`, `memcmp`, `memchr` - PASS
- `memmove` correctly handles overlapping buffers.
- `memcpy` moves words and relies on unaligned access, which the ISA permits.

### Standard Library (stdlib.h) - WORKING ✅

- `malloc`, `free`, `calloc`, `realloc` - segregated free lists; the heap is sized by `s32-ld --heap-size`.
- `abs`, `labs`, `atoi`, `atol` - PASS
- `rand`, `srand` - PASS
- `exit`, `abort`, `atexit` - PASS (MMIO and DEBUG variants)
- `qsort`, `qsort_r`, `bsearch` - PASS
- `strtol`, `strtoul`, `strtoll`, `strtod` (and the `_l` forms) - PASS
- `div`, `ldiv` - PASS
- `getenv` and the environment (MMIO)

### Standard I/O (stdio.h) - WORKING ✅

- `printf` family with integer and floating-point conversions (see [PRINTF_STATUS.md](PRINTF_STATUS.md))
- `sscanf`, `vsscanf` (MMIO build); `scanf` and `fscanf` are declared but
  not implemented
- `fopen`, `fclose`, `fread`, `fwrite`, `fdopen`, `tmpfile`, `getline`, `setvbuf` (honours its size) - buffered, MMIO-based
- `fmemopen`, `open_memstream`
- `stat`, `fstat`, directories (`dirent.h`) - MMIO-based

The full stdio is in `libc_mmio.s32a`; `libc_debug.s32a` carries a minimal
console-only stdio over the DEBUG instruction.

### Math Library (math.h) - WORKING ✅

- The ISA has f32 and f64 instructions (since 2026-02-01); `sqrt` and `fabs`
  compile to them (`math_hw.c`).
- Transcendentals and the rest of `math.h` are C in `math_soft.c`
  (plus `math_fma.c`, `math_cordic.c`), compiled to those instructions.
  slow32-dbt and QEMU may replace hot ones with host calls.
  [MATH_COMPARISON.md](MATH_COMPARISON.md) compares the Taylor and CORDIC versions.
- Integer helpers remain in `math_int.c` (`iabs`, `imin`, `imax`, `isqrt`, `ipow`).

### Other

- `time.h` (`time`, `clock_gettime`, `mktime`, `strftime`, time zones via the host), `setjmp`/`longjmp`, `signal`/`raise`, `getopt`/`getopt_long`, `errno`/`strerror`
- `locale.h`: `setlocale`, `newlocale` and the `_l` functions; named locales with collation in `locales.s32o` (an object a program links ahead of libc)
- POSIX extras over MMIO: sockets (IPv4 TCP; `socket.h`, `netdb.h`), `poll`, process `exec`, terminal control (`term.h`, `termios.h`), the TUBE display (`tube.h`), DPC timers (`s32dpc.h`)
- C++ support: the unwinder is in libc; `libs32cxx.s32a` is the freestanding C++ run time (exceptions, RTTI, `new`/`delete`). Full libc++/libc++abi builds on top (`regression/run-libcxx.sh`).

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

Later backend work is recorded in [IMPROVEMENTS.md](IMPROVEMENTS.md) and
`llvm-backend/SLOW32/STATUS.md`.

## File Structure
The sources are listed, by archive, in `runtime/Makefile`:

- `libs32.s32a` - compiler support: intrinsics, 32- and 64-bit division,
  `math_hw`/`math_soft`/`math_fma`/`math_cordic`, `builtins.c`
- `libc_mmio.s32a` - the C library with MMIO I/O (`stdio.c`,
  `stdio_buffered.c`, `*_mmio.c`, ...)
- `libc_debug.s32a` - the same core with DEBUG-instruction console I/O and
  stubs (`*_stub.c`) for the host services
- `libs32cxx.s32a` - the C++ run time (`cxa_*.c`, `cxa_std.cpp`)
- `crt0.s32o`, `locales.s32o` - objects, not archive members

## Build and Link Order
The recommended link order is:
`s32-ld --mmio 64K -o prog.s32x crt0.s32o user.s32o libc_mmio.s32a libs32.s32a`

Use `libc_debug.s32a` instead of `libc_mmio.s32a` (and no `--mmio`) for simple console output via the `DEBUG` instruction without MMIO infrastructure requirements.
