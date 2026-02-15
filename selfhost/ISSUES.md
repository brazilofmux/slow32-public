# Code Review Issues: SLOW-32 Self-Hosting Toolchain

This document captures bugs, inconsistencies, and opportunities for improvement identified during the code review of the `./selfhost` directory.

## Stage 0: Emulator (`s32-emu.c`)

### 1. [BUG] Pathname Truncation in `OP_MMIO_OPEN`
The implementation of `OP_MMIO_OPEN` (and `OP_MMIO_STAT`) incorrectly truncates the last character of the provided pathname:
```c
uint32_t plen = (length < sizeof(path_buf)) ? length : sizeof(path_buf) - 1;
memcpy(path_buf, data + off, plen);
path_buf[plen] = '\0';
if (plen > 0) path_buf[plen - 1] = '\0'; // <--- BUG: Truncates last char
```
If the guest provides a non-NUL-terminated string (standard for Forth), the last character of the filename is lost. If it *is* NUL-terminated, the truncation is redundant but safe. This makes opening files with 1-character names impossible and potentially breaks all file I/O for standard Forth strings.

### 2. [BUG] `OP_MMIO_STAT` Layout Overlap and Buffer Size
The `ctime` field in the `stat` result layout is corrupted and the buffer is too small:
```c
/* Write s32_mmio_stat_result_t (104 bytes, packed) */
uint8_t buf[104]; // <--- BUG: s32_mmio_stat_result_t is 112 bytes
...
/* ctime */
tmp = (uint64_t)st.st_ctim.tv_sec;  memcpy(buf + 96, &tmp, 8);
t32 = (uint32_t)st.st_ctim.tv_nsec; memcpy(buf + 104 - 4, &t32, 4); // <--- BUG: Overwrites buf[100..103]
```
`buf + 104 - 4` is `100`. Since the `sec` field at `96` is 8 bytes long, it occupies `96..103`. Writing `nsec` at `100` overwrites the upper 4 bytes of `sec`. The correct offset for `nsec` is `104`, requiring a buffer of at least 108 bytes (or 112 for full alignment).

### 3. [CRITICAL] Lack of Memory Bounds Checking
The emulator performs no bounds checking on guest memory accesses (`rd32`, `wr32`, `LDB`, `STW`, etc.). A malicious or buggy guest program can read/write anywhere in the host process's memory space.

### 4. [STABILITY] Stack Allocation Risk in `load_s32x`
`load_s32x` allocates memory based on the header's `mem_size` but does not ensure it covers the `stack_base`. If `stack_base` is outside the allocated `mem_size`, the emulator will segfault on the first stack access.

### 5. [PORTABILITY] Host Endianness Dependency
The memory access helpers (`rd32`, `wr32`) use `memcpy` directly into host integers, making the emulator non-portable to Big Endian hosts.

---

## Stage 2: Forth Assembler (`asm.fth`)

### 6. [FIXED] `.byte` and `.word` Limited to Single Values
This was resolved in the v2 relocation (`selfhost/v2/stage01/asm.fth`). `DO-BYTE` and `DO-WORD` now use a `BEGIN ... AGAIN` loop to process all tokens on a line.

### 7. [INCONSISTENCY] PC-Relative Reference Points
The reference point for PC-relative offsets differs between instructions:
- `JAL` (0x40): Relative to `PC`.
- `BEQ/BNE/...` (0x48+): Relative to `PC + 4`.
While the assembler compensates for this in `PARSE-TARGET` vs `PARSE-BTARGET`, it is an inconsistent design that complicates manual assembly and debugging.

---

## Stage 4: C Compiler (`cc.fth`)

### 8. [FIXED] String Truncation in Self-Host
This was resolved by the fix to Issue #6. The Stage 2 assembler now correctly parses comma-separated byte lists emitted by the C compiler.

### 9. [DOC] Misleading Type Encoding Comments
Comments in `cc.fth` claim that `TY-STRUCT` indices and array counts overlap in the type word (bits 16-31), but the code actually uses disjoint ranges (14-21 for structs, 22-31 for arrays). The comments should be updated to reflect the actual (correct) implementation.

### 11. [FIXED] Dead Code and Redundancy in `EMIT-LI-R2`
This was cleaned up during the v2 relocation. Both `EMIT-LI-R1` and `EMIT-LI-R2` now use identical, optimized logic without redundant temporary variables.

---

## Stage 6: Subset C Archiver (`s32-ar.c`)

### 12. [SPIKE] Hardcoded Limitations in `s32-ar.c`
The current `selfhost/v2/stage04/validation/s32-ar.c` is a minimal spike with several hardcoded limitations:
- It only supports exactly one object file in an archive.
- The member name in the archive is hardcoded to "divsi3.s32o" regardless of input filename.
- It ignores the command argument (e.g., 'c', 'rc').
This must be replaced with a proper subset implementation before Stage 6 can be considered complete.

---

## Bootstrap Flow / Runtime Artifacts

### 15. [BOOTSTRAP] Stage04+ Depends on Seeded Runtime Binaries
Current Stage04 regression expects these prebuilt runtime artifacts to already exist:
- `runtime/crt0.s32o`
- `runtime/libc_mmio.s32a`
- `runtime/libs32.s32a`

For new users (or clean environments like Raspberry Pi), copying these files from another machine works as a temporary compromise, but it is not aligned with the long-term self-hosting goal.

Target direction:
- Provide a selfhost-built runtime path under `./selfhost` that can be produced with Stage01/Stage02 tools (assembler + archiver), starting from minimal runtime needs.
- Make Stage04 regression consume that selfhost runtime path by default (or via explicit flag), with clear fallback to seeded runtime when desired.

Acceptance criteria (future work):
- A clean checkout can run Stage00 -> Stage04 using only:
  - host C compiler for Stage00 `s32-emu`
  - seeded `forth/kernel.s32x` + `forth/prelude.fth`
  - no copied runtime `.s32o/.s32a` blobs from another machine
- `selfhost/v2/stage04/run-regression.sh` fails fast with clear missing-artifact guidance if runtime prerequisites are absent.

---

## Documentation Opportunities

### 10. `ISA-ENCODING.md` Errors and Discrepancies
- The "Operation" column for `SLTI` is a copy-paste error from `ADDI` (or rather, `SLTI` is missing from the I-Type table while appearing in other sections).
- Opcode ranges for R-type vs I-type are not clearly defined. Instructions `0x18` (SGT) through `0x1D` (SGEU) are R-type (using `rs2`) but reside in the `0x10-0x1F` range typically reserved for I-type arithmetic.
- **Major Discrepancy:** The instruction format diagrams show `funct7` (bits 25-31) and `f3` (bits 12-14) fields, but the Stage 0 emulator (`s32-emu.c`) and Forth assembler (`asm.fth`) use a flat 7-bit opcode space, effectively ignoring or zeroing these fields. This should be clarified to avoid confusion for developers familiar with standard RISC-V.

### 13. [DOC] Stale Implementation Status in `STAGE0-EMULATOR.md`
The `STAGE0-EMULATOR.md` file lists several instructions as "deferred" or "not needed by Forth kernel" (e.g., `DIV/REM`, `BLTU/BGEU`, `LDH/STH`, `MULH`), but these are already fully implemented in the actual `s32-emu.c` source. The documentation should be updated to reflect that the Stage 0 emulator is more capable than initially planned, while still maintaining the "intentional subset" philosophy (no floating-point, no 64-bit integers).

### 14. [DOC] Intentional Subset Alignment
While `./docs/INSTRUCTION-SET.md` defines a rich set including floating-point and 64-bit conversions, the `./selfhost` toolchain intentionally targets a minimal integer-only subset. This boundary is mostly respected, but the documentation in `selfhost/docs/` should more explicitly cross-reference the main ISA spec to clarify that self-hosting is a "reduced surface" effort.
