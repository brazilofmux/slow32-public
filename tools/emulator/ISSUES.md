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

### 5. Loader: Implicit BSS Zeroing (Not a Bug)
The `.s32x` loader currently assumes memory starts zeroed, which happens today because the emulator uses `mmap(MAP_ANONYMOUS)` or `calloc`.

- **Status**: Not a bug. The contract is that the guest runtime must zero its own `.bss` (like real hardware), and `crt0.s` already clears `.bss` via `memset`.
- **Note**: Emulator zeroed memory is a convenience, not a required guarantee.

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

### 11. QEMU Backend Missing `READ_DIRECT` (0x0C) (Resolved)
`qemu-backend/target/slow32/mmio.c` was missing `READ_DIRECT` (0x0C).

- **Status**: Fixed. Added in the same commit that brought FTRUNCATE, service
  negotiation, and the term service to the QEMU backend.

### 12. QEMU Fused Compare-Branch Clobbered Its Own Operand (Resolved)

**Severity**: silent wrong answer, `qemu-system-slow32` only, default machine.
Every negative `%d`/`%lld` printed by the self-hosted libc came out with a
doubled sign — `sprintf("%lld", -1)` gave `--1`.

`printf_enhanced.c` hoists an already-present sign into a prefix:

```c
if (is_negative && conv_str[0] == '-') {
    /* already has minus sign */
} else if (is_negative && is_signed_conv) {
    prefix[prefix_len++] = '-';
}
```

Under QEMU the first test was false even though `conv_str[0]` *was* `'-'`, so
the sign was added a second time. The magnitude was always right, which ruled
out the 64-bit paths — a probe confirmed the negation itself was correct
(`neg_lo=1 neg_hi=0`, matching the interpreter).

**Root cause**: `slow32_emit_cmp_and_branch()` in `target/slow32/translate.c`.
`load_gpr()` returns the `cpu_regs[]` TCG global *itself*, not a copy, so
materializing the queued compare into `rd` clobbers `lhs`/`rhs` whenever `rd`
aliases one of them. The fused branch then tested the freshly stored 0/1
instead of the original operand. The trigger is a same-register compare:

```
xori r3, r10, 45     ; r3 = (c ^ '-')  -> 0 when equal
seq  r3, r3, r0      ; queued: rd == lhs == r3
beq  r3, r0, .L8     ; fuses -> branches on the CLOBBERED r3
```

`slow32_flush_pending_cmp()` (the unfused path) was always safe, because
`store_cond()` computes into a temp before assigning `rd`; only the fused path
reads its operands *after* that assignment. Fix: snapshot an operand that
aliases `rd` into a temp before storing the condition.

**Why nothing caught it**: clang emits a compare-and-branch here; only the
stage08 self-hosted compiler emits the `xori`/`seq`/`beq` triple with
`rd == lhs`. The clang differential passed with QEMU in the roster throughout.
Found on the first full run of `regression/run-kit-differential.sh`, which
builds the stage08 corpus with the kit toolchain and diffs every engine —
44/44 agree after the fix, and the clang suite is unchanged (the same four
known qemu intrinsic-bounds fault-reporting divergences, nothing new).

This is the second bug of the same family found the same day: DBT-15 was the
x86-64 translator mistranslating `bge zero, rX`, also a selfhost-only pattern,
also a fused/special-cased compare path. Both were invisible to a suite built
entirely from clang output.

### 13. Memcheck mode: `slow32 -m` (2026-11-11)

Not a defect: a mode.  `memcheck.h` keeps addressability per byte and
definedness per bit beside the reference interpreter's memory and
registers, and reports uses of what is not the program's or not yet a
value: a branch, an address or an output byte that depends on an
uninitialised one, a load or store in a dead frame, in a freed block, in
the padding after a block or through a null pointer, a free of what is not
a live block.  The header says what is seen and what is not (overruns
between globals or within a frame are not).  The program runs the same
instructions with or without it; a clean run prints nothing.

Gate: `regression/run-memcheck.sh` -- seeded errors that must each be told
for what they are, by clang and by the kit, and whole suites run under
`S32_MEMCHECK=1` that must pass as they do without and report nothing.

Three things it had to learn from real programs, each a false report first:

- the HIR backends' large-frame prologue saves lr and fp at sp-4 and sp-8
  before it moves sp: eight bytes below sp are the program's;
- the same backends select without a branch, `y ^ ((x ^ y) & m)`, and bit by
  bit that is undefined whenever the side not chosen is: the two XORs and
  the AND are followed as one select;
- the Forth kernel's threaded code is data in its code segment, read by
  design: a null pointer is told by the base register holding 0, not by the
  address being low.

Known reports, listed in `regression/memcheck/known.txt`: six hand-written
assembly tests that keep scratch below the stack pointer, and islisp's
conservative collector, which reads every word of the stack on purpose.

The long sweeps pass too (`run-memcheck.sh --all`: cobol, ccvs, the f77
corpus, xbase, xbc), after two things about the suites rather than the
mode.  The COBOL suite's CCVS both-paths check compares console output
byte for byte, and the interpreter's closing lines carry the wall time: the
sweep runs the suite with CCVS=0 and CCVS by itself, quiet, against its
recorded totals.  (islisp's suite was left out here for a day, on the
belief that the kit's libm differed from clang's on the interpreters; it
was a stage08 binary twelve days old, runtime ISSUES-29.  It is swept, and
its collector's reports are the known ones.)  One corpus program is listed as known:
gfortran's whole_file_14.f90 follows a pointer to a local of a subroutine
that has returned, on purpose.
