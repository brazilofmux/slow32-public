# Code Review Issues: SLOW-32 Self-Hosting Toolchain

This document captures bugs, inconsistencies, and opportunities for improvement identified during the code review of the `./selfhost` directory.

## Stage 0: Emulator (`s32-emu.c`)

### 1. [FIXED] Pathname Truncation in `OP_MMIO_OPEN`
The spurious `path_buf[plen - 1] = '\0'` line has been removed. Both OPEN and STAT paths
now correctly NUL-terminate at `path_buf[plen]`.

### 2. [FIXED] `OP_MMIO_STAT` Layout Overlap and Buffer Size
Buffer is now 112 bytes. `ctime.tv_nsec` is written at offset 104, no longer overlapping
`ctime.tv_sec` at offset 96.

### 3. [FIXED] Missing Centralized Guest Memory Bounds Checks
Stage0 now routes guest memory accesses through centralized checked helpers:
- `mem_check()` for range validation and fault reporting
- `mem_read16()/mem_read32()` for checked loads
- `mem_write32()` for checked stores

These checks are now applied to instruction fetch, load/store instructions, and MMIO
ring metadata/descriptor reads and writes. Out-of-bounds guest accesses now halt cleanly
with a deterministic fault instead of touching host memory out of range.

### 4. [FIXED] Stack Allocation Risk in `load_s32x`
`load_s32x` now computes total memory as the max of header `mem_size`, `stack_base`,
and MMIO end (when MMIO is enabled). This prevents under-allocation when older artifacts
under-report `mem_size` relative to the requested stack top.

### 5. [NOT SUPPORTED] Host Endianness Dependency
The memory access helpers (`rd32`, `wr32`) use `memcpy` directly into host integers.
While this makes the emulator non-portable to Big Endian hosts, the performance benefits
of native-endian memory mapping on common architectures (x86_64, ARM64, RISC-V) outweigh
the cost of BE portability. The emulator and toolchain now explicitly check for host
endianness at startup and will exit with an error on Big Endian platforms.

---

## Stage 2: Forth Assembler (`asm.fth`)

### 6. [FIXED] `.byte` and `.word` Limited to Single Values
This was resolved in the v2 relocation (`selfhost/stage01/asm.fth`). `DO-BYTE` and `DO-WORD` now use a `BEGIN ... AGAIN` loop to process all tokens on a line.

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

### 12. [NARROWED] Stage02 `s32-ar.c` Needs Full Command Coverage
The Stage02 subset archiver (`selfhost/stage01/validation/s32-ar.c`) now supports bounded multi-member create and real `rc` replace-on-existing behavior with basename matching.

Remaining gap:
- Expand command surface and parity checks for `d/m/v/p` paths before Stage02 can be considered complete.

---

## Bootstrap Flow / Runtime Artifacts

### 15. [RESOLVED] Stage04+ Depends on Seeded Runtime Binaries
Stage04 regression scripts now assemble the bootstrap runtime from in-tree sources at
test time, using the Forth assembler (stage01). No prebuilt `runtime/*.s32o` or
`runtime/*.s32a` blobs are required. A clean checkout can run Stage00 through Stage04
using only:
- host C compiler for Stage00 `s32-emu`
- seeded `forth/kernel.s32x` + `forth/prelude.fth`

Runtime sources: `selfhost/stage01/crt0_minimal.s`, `selfhost/stage01/mmio_minimal.s`.
These are linked as direct objects (not archives) to work around Issue #16.

### 16. [FIXED] Forth Archiver Symbol Index Bugs
The Forth archiver (`selfhost/stage01/ar.fth`) had two bugs preventing archive-based
linking from working:

**Bug A: `OUTSTR-ADD` stack underflow** — In the output string table builder,
`R>` popped the string length from the return stack for the NUL-terminator write,
leaving the stack empty for the subsequent size update. Fixed by using `R@` (copy)
instead of `R>` (pop), then properly advancing `out-strsz` with `R> 1+ out-strsz +!`.

**Bug B: `BUILD-SYMINDEX` used wrong loop variable** — In the nested `?DO` loops,
`J sym-j !` stored the outer loop index (member index) where the inner loop index
(symbol index) was needed. This caused every symbol within a member to read the same
symbol table entry (at position `member_idx * 16 + sym_offset` instead of
`symbol_idx * 16 + sym_offset`). Fixed by replacing `J sym-j !` /
`sym-j @ 16 * m-tmp-off @ +` with `I 16 * m-tmp-off @ +`.

Both fixes verified by stage01 regression tests. Archive-based linking
now works correctly in the stage02 selfhost libc build.

---

## Stage 8: Subset C Compiler (`cc-min-pass1.c`)

### 17. [FIXED] `&&` and `||` Are Not Short-Circuiting
`&&` and `||` now short-circuit in `parse_binop`: LHS is normalized to 0/1, then a
conditional branch skips the RHS when the result is already determined. Verified by
`min_short_circuit.c` test using side-effect counters.

### 18. [FIXED] `INT_MIN` Handling in `emit_num`
Special-cased `INT_MIN` (-2147483648) to emit the literal string directly before
the negate-and-loop path. Uses `-2147483647 - 1` in the comparison to avoid
C literal overflow ambiguity.

### 19. [FIXED] Lack of Pointer Arithmetic Scaling
`ptr + int`, `int + ptr`, `ptr - int`, and `ptr - ptr` now scale by element size.
`++`/`--` and `+=`/`-=` on pointers also scale correctly.
Verified by `min_ptr_arith.c` test (int* advances by 4, char* by 1, ptr-ptr divides).

### 20. [FIXED] Small Fixed-Size Symbol Tables and Buffers
All limits bumped to support self-hosting-scale programs:
- `MAX_LOCALS` 64→128, `MAX_GLOBALS` 64→256, `MAX_FUNCS` 128→256
- `NAMESZ` 32→48, name buffers proportionally increased
- `MAX_OUTPUT` 65536→131072 (128KB), `MAX_STRINGS` 128→512, `STR_POOL_SZ` 4096→16384

### 21. [CC] Caller-Side Argument Limit
The compiler only supports up to 8 arguments in function calls, passed via registers `r3-r10`.
```c
while (k > 0) {
    k = k - 1;
    emit("    ldw r"); emit_num(3 + k); emit(", r29, 0\n    addi r29, r29, 4\n");
}
```
If a function is called with more than 8 arguments, it will attempt to use non-existent or incorrect registers (`r11+`). The calling convention requires overflow arguments to be passed on the stack.

### 22. [FIXED] Hardcoded Stack Frame Size
`emit_prologue` now emits a placeholder that is back-patched after the function body is
parsed. `emit_prologue_final(frame_sz)` overwrites the placeholder with the actual frame
size, computed as `((-local_offset + 15) & ~15)` with a minimum of 32 bytes. This
eliminates the fixed 256-byte frame overhead and prevents stack overflow for functions
with many locals.

### 23. [INCOMPLETE] Comma Operator and `for` Loop Expressions
The `for` loop parser only supports a single expression in the initialization, condition, and increment sections. Standard C allows multiple expressions separated by the comma operator, which is frequently used in `for` loops.

### 24. [RESOLVED] Global and Local Initialization Limitations
Global variables are only allocated in `.bss` with `.space`; they cannot be initialized
at declaration. Local array initialization is also unsupported. However, BSS zero-init
is sufficient for the selfhost toolchain, and all required initialization is done via
assignment statements.

### 25. [FIXED] Empty Error Reporting
`cc_error` now prints `cc-min:<line>: error: <msg>` to stderr using `fputs`/`fput_uint`.
Example output: `cc-min:5: error: unexpected character`.

## Stage 4: C Compiler (`cc.fth`) — Long-Call Materialization Bug

### 17. [FIXED] Second Call to Auto-Declared Function Generates Variable-Access Code

**Fixed in commit 5b0a9b1** by adding `GSYM-KIND-FUNC?` helper that accepts gsym-kind=1
(definition) OR gsym-kind=2 (prototype) when resolving identifiers in call position.

**Root cause:** When cc.fth auto-declares an external function on first use, it adds the
symbol with gsym-kind=2 (prototype). On subsequent references, the identifier resolver
checked `gsym-kind = 1` (definition only), falling through to the global-variable-access
path (lui+addi+ldw) instead of the function-call path (lui+jalr or call). This caused
the second call to load instruction bytes as data and jump to a garbage address.

**Tracked repro:** `selfhost/stage01/tests/subset-known-gaps/subset_gap01_implicit_repeat_call.c`

**Verification:** stage02 cc-min now calls fopen/fclose/fgetc/fputc/fputs directly
(no io_* wrappers needed); all 29 tests pass.

### 30. [FIXED] cc.fth Clamps Global Array Sizes to 65535 Bytes
`TYPE-SET-ARRAY` stored the array element count in bits 16-31 of the type word (16 bits,
max 65535). For `uint8_t`/`char` arrays exceeding 65535 elements, the count was silently
clamped, causing undersized `.space` directives in generated assembly.

**Impact:** The stage02 assembler (`s32-as.c`) declared `uint8_t g_text[262144]` which
was emitted as `.space 65535`. When assembling cc-min (67KB .text), bytes past offset
65536 overflowed into `g_data`, corrupting the object file.

**Fix:** Added `big-arr-count` sideband variable in cc.fth. `TYPE-SET-ARRAY` saves the
full 32-bit count before clamping. Both `.space` emission sites compute the real byte
size as `element_size * big-arr-count` when the type is an array.

Also bumped `MAX_BSS` in `s32-as.c` from 256KB to 8MB to accommodate the archiver's
4MB data buffer.

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

---

## Recent Commit Review Follow-Ups

### 26. [RESOLVED] Missing `minizork.z3` Broke Zork Tests
Commit `def868a` removed `zork/stories/minizork.z3`, while `zork/tests/run-tests.sh`
requires that exact path. The story file has now been restored in commit `694ed6a`
(`Restore minizork story blob for zork tests`).

### 27. [FIXED] Stage02 cc-min Default Emulator Priority
`choose_default_emu()` in `run-cc-spike.sh` now prefers `slow32-dbt` before `slow32-dbg`.

### 28. [RESOLVED] Unused Locals in `cc-min-pass1.c`
`lsc` and `rsc` are declared and used in `emit_ptr_add()` and `emit_ptr_sub()`, not in
`parse_binop()`. No unused locals exist; this was a false positive.

### 29. [DOC] Commit Message vs Touched Files Audit Trail
Commit `a851552` message says it fixes issues `#1/#2/#19/#25`, but touched files are:
- `selfhost/ISSUES.md`
- `selfhost/stage01/cc.fth`
- `selfhost/stage02/*`

No Stage0 emulator source file is touched in that commit. If Stage0 fixes were landed
earlier, the message should clarify that this commit updates tracking/docs for #1/#2
rather than containing those code changes directly.
