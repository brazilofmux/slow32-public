# SLOW-32 Runtime — Issues & Recommendations

This document tracks bugs, architectural limitations, and opportunities for improvement in the SLOW-32 C runtime and standard library.

## Critical Bugs & Safety Issues

### 1. `printf` / `fprintf` Silent Truncation (Resolved)
Both `printf` and `fprintf` used a local `buffer[1024]` on the stack.
- **Status**: Fixed in `903c892`. `vsnprintf_enhanced` now supports size-querying (two-pass), and the formatting functions dynamically allocate a heap buffer if the output exceeds the stack limit.

### 2. Missing `errno` Wiring (Resolved, refined 2026-08)
The `errno` variable existed but was not set by MMIO-based system calls.
- **Status**: Fixed initially in `903c892` (EINTR / ERR→EIO). Refined 2026-08:
  hosts put a positive errno in the response `length` field on
  `S32_MMIO_STATUS_ERR`; `mmio_request.c` maps it into guest `errno`
  (ENOENT, EBADF, EINVAL, …) with EIO fallback.
- **Open refinement (2026-08 review)**: the emulator passes *raw host* errno
  values, so the guest sees whatever the host OS numbers them. This works
  for the common set because macOS and Linux agree there (ENOENT=2, EBADF=9,
  EACCES=13, EINVAL=22), but higher values diverge (EAGAIN is 35 on macOS,
  11 on Linux), so guest code comparing against those is host-dependent. A
  canonical `S32_ERRNO_*` enum in `mmio_ring_layout.h` — host maps
  host→canonical in `mmio_fail()`, guest maps canonical→its `errno.h` — would
  make the wire format host-independent. Low urgency: no current guest code
  tests errnos outside the agreeing set.

### 3. `pow()` Arbitrary Fast-Path Limit (Resolved)
The `pow(x, y)` implementation had an arbitrary fast-path limit for integer exponents.
- **Status**: Fixed in `43a7926`. Removed the `y < 100.0` limit. Binary exponentiation is now used for any integer exponent, providing significant performance gains for large exponents.

### 4. `math.c` Precision Loss for Large $x$
Trigonometric functions use `fmod(x, TWO_PI)` for range reduction.
- **Problem**: For very large values of $x$, `fmod` loses significant precision because `TWO_PI` is not exactly represented.
- **Recommendation**: Implement a more robust range reduction algorithm (like Payne-Hanek).

---

## Performance & Optimization Opportunities

### 5. Hardware FP Instruction Emission (Resolved)
The SLOW-32 ISA provides native `FSQRT.S`, `FSQRT.D`, `FABS.S`, and `FABS.D` instructions.
- **Status**: `math_hw.c` now defines `sqrt`/`fabs`/`sqrtf`/`fabsf` via `__builtin_*` and is compiled with `-fno-builtin -fno-math-errno`, ensuring `FSQRT.*` and `FABS.*` are emitted.
- **Note**: `math_soft.c` contains the remaining libm implementations and is compiled with `-fno-builtin` to avoid recursive lowering.

### 6. Slow BSS Clearing in `crt0.s` (Resolved)
`crt0.s` cleared the BSS section byte-by-byte.
- **Status**: Fixed in `43a7926`. Replaced manual loop with `jal memset`. This allows fast emulators (`slow32-dbt`, `QEMU`) to intercept the call and use native host `memset`, and slow emulators to use the word-optimized `memset` from `intrinsics.s`.

---

## Architectural Opportunities (MMIO & CORDIC)

### 7. CORDIC Math Optimization (Guest-side) (Resolved)
- **Status**: Fixed in `[COMMIT_HASH]`. CORDIC is now the default implementation for `sin`, `cos`, `atan2`, and `exp`.
- **Benefit**: Since `MUL` takes 32 cycles on SLOW-32, CORDIC (using only 1-cycle shifts and adds) is significantly faster. It also reduced the `math_soft.o` binary size by ~12%.
- **Context**: Inspired by the performance gap between classic 8-bit BASICs and OS/9's BASIC09.

### 8. Security Engine (MMIO)
- **Opportunity**: Add MMIO opcodes for `AES-256`, `SHA-256`, and `Ed25519`.
- **Benefit**: Reimplementing crypto in the guest is slow and highly vulnerable to side-channels. Offloading to host-native routines via MMIO provides security, verification, and speed.
- **Implementation**: Define a standard "Security Engine" opcode range (`0x50-0x5F`).

### 9. High-Resolution `TIMER`
The current `TIMER` uses 1-second resolution `time()`.
- **Opportunity**: Use the `GETTIME` (0x30) opcode to provide a microsecond-resolution timer for benchmarking.

### 10. `READ_DIRECT` (Zero-copy I/O)
- **Opportunity**: Ensure all emulators support this opcode to bypass the `S32_MMIO_DATA_BUFFER` for large reads, improving throughput significantly.

### 11. `rewinddir` was a no-op (Resolved 2026-08)
- **Status**: Fixed. Guest issues `S32_MMIO_OP_REWINDDIR` (0x2B); host
  emulators (`mmio_ring.c`, QEMU `mmio.c`) call POSIX `rewinddir` on the
  open `DIR*`.

### 12. `free` double-free freelist corruption (Resolved 2026-08)
- **Status**: Fixed. `free` rejects out-of-heap pointers, already-free
  blocks, and impossible sizes before coalescing.
