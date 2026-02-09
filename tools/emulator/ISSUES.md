# SLOW-32 Emulator — Issues & Recommendations

This document tracks bugs, architectural risks, and opportunities for improvement in the SLOW-32 emulator suite (`slow32`, `slow32-fast`, and the MMIO subsystem).

## Critical Bugs & Safety Issues

### 1. Out-of-Bounds Register Access for `f64` (Resolved)
The `LOAD_F64` and `STORE_F64` macros/functions access `cpu->regs[reg]` and `cpu->regs[reg + 1]`.
- **Status**: Fixed. Added `S32_CHECK_F64_REGS` diagnostic flag (same pattern as `S32_TRAP_ON_UNALIGNED`). Enabled by default in `slow32` (reference emulator), disabled by default in `slow32-fast` (enabled via `-D S32_STRICT_ALIGNMENT`). Checks that f64 register index is even and < 31; reports PC on violation.

### 2. Unchecked Memory Allocations (Host)
Multiple call sites use `malloc`, `calloc`, or `strdup` without checking for a `NULL` return value.
- **Affected sites**: `cpu_init_mmio` (allocating `mmio_ring_state_t`), `load_s32x_file` (string table and section buffers), and `mmio_ring.c` (path and environment blobs).
- **Recommendation**: Audit all host-side allocations and ensure they handle OOM by halting the emulator or returning an error.

### 3. MMIO Host Resource Exhaustion
Opcodes like `S32_MMIO_OP_OPEN`, `S32_MMIO_OP_STAT`, and `S32_MMIO_OP_GETENV` call `malloc` on the host side for every request.
- **Problem**: A malicious or buggy guest program can repeatedly trigger these requests with large length fields, potentially exhausting host memory or causing significant fragmentation.
- **Recommendation**: Impose strict limits on the number of open files and the maximum size of path/name strings processed by MMIO.

### 4. `mmio_ring.c` Memory Access Gaps (Not an Issue)
The `process_request` function uses `req->offset % S32_MMIO_DATA_CAPACITY`.
- **Status**: Already validated. All MMIO operations check `offset + length <= S32_MMIO_DATA_CAPACITY` and return `S32_MMIO_STATUS_ERR` on overflow.

### 5. Loader: Implicit BSS Zeroing
The `.s32x` loader assumes that the emulator has already zeroed all memory.
- **Problem**: While `mmap(MAP_ANONYMOUS)` typically provides zeroed pages, relying on this is fragile and may not hold for all memory management strategies or future extensions.
- **Recommendation**: Explicitly zero the difference between `mem_size` and `size` for each section in `load_s32x_file`.

---

## Architectural Risks & Performance

### 6. Masked Alignment Bugs
By default, the emulators run on x86-64 which handles unaligned loads and stores in hardware.
- **Problem**: SLOW-32 is a strict alignment ISA. Toolchain bugs that produce unaligned code/data may go unnoticed during emulation, only to fail on strict hardware or more pedantic emulators.
- **Recommendation**: Enable `S32_TRAP_ON_UNALIGNED` by default in the reference `slow32` implementation.

### 7. Inconsistent MMIO Access Widths (Not an Issue)
`slow32.c` and `slow32-fast.c` were claimed to use different sub-word MMIO logic.
- **Status**: Both emulators already use identical read-modify-write logic for byte/halfword MMIO stores.

### 8. `slow32-fast.c` Instruction Prefetch Risks
The fast emulator caches the code region pointer for speed.
- **Problem**: If the memory map changes during execution (e.g., via a future `mmap` syscall), the cached pointer may become stale.
- **Recommendation**: Implement a mechanism to invalidate the code region cache when the memory manager's regions are modified.

---

## Quality of Life & Diagnostics

### 9. Obsolete `slow32_mmio.c` (Resolved)
This file was a truncated copy of `slow32.c` and was not part of the build process.
- **Status**: Fixed. File removed.

### 10. Vague Memory Fault Messages (Resolved)
Read fault messages lacked PC and SP context that write faults already included.
- **Status**: Fixed. Read faults in `slow32.c` now include `PC=` and `SP=` to match write fault format.
