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

### 12. [NARROWED] Stage06 `s32-ar.c` Needs Full Command Coverage
The Stage06 subset archiver (`selfhost/stage04/validation/s32-ar.c`) now supports bounded multi-member create and real `rc` replace-on-existing behavior with basename matching.

Remaining gap:
- Expand command surface and parity checks for `d/m/v/p` paths before Stage06 can be considered complete.

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
The Forth archiver (`selfhost/stage02/ar.fth`) had two bugs preventing archive-based
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

Both fixes verified by stage02 and stage03 regression tests. Archive-based linking
now works correctly in the stage05 selfhost libc build.

---

## Stage 8: Subset C Compiler (`cc-min-pass1.c`)

### 17. [BUG] `&&` and `||` Are Not Short-Circuiting
The implementation of `TK_LAND` and `TK_LOR` in `emit_binop` evaluates both sides of the expression before performing the logical AND/OR.
```c
else if (tok == TK_LAND) emit("    sne r2, r2, r0\n    sne r1, r1, r0\n    and r1, r2, r1\n");
```
In standard C, these operators MUST short-circuit. This bug can lead to side effects occurring when they shouldn't (e.g., `if (p && *p)`) and violates the principle of least surprise for a C compiler.

### 18. [BUG] `INT_MIN` Handling in `emit_num`
The `emit_num` helper fails to correctly handle `INT_MIN` (-2147483648).
```c
if (v < 0) { neg = 1; v = 0 - v; }
...
while (v > 0) { ... }
```
When `v` is `INT_MIN`, `0 - v` is still `INT_MIN` due to two's complement overflow. The `while (v > 0)` loop will not execute, and the function will only emit a minus sign.

### 19. [LIMITATION] Lack of Pointer Arithmetic Scaling
While array indexing `[]` correctly scales by element size, general pointer arithmetic (e.g., `p + 1`) in `parse_binop` does not.
```c
static void parse_binop(int min_prec) {
    ...
    emit_binop(op);
    g_lval = 0; g_expr_type = TY_INT;
}
```
All binary operations currently result in `TY_INT` and perform raw integer arithmetic. Adding 1 to a `int *` will only advance it by 1 byte instead of 4.

### 20. [SCALABILITY] Small Fixed-Size Symbol Tables and Buffers
The compiler uses small fixed-size arrays for its symbol tables and buffers:
- `MAX_LOCALS 64`, `MAX_GLOBALS 64`, `MAX_FUNCS 128`
- `NAMESZ 32` (truncates identifiers longer than 31 characters)
- `g_output[65536]` (64KB limit for generated assembly)
These limits are sufficient for small test cases but will likely be exceeded when attempting to compile the full toolchain or larger programs during self-hosting.

### 21. [CC] Caller-Side Argument Limit
The compiler only supports up to 8 arguments in function calls, passed via registers `r3-r10`.
```c
while (k > 0) {
    k = k - 1;
    emit("    ldw r"); emit_num(3 + k); emit(", r29, 0\n    addi r29, r29, 4\n");
}
```
If a function is called with more than 8 arguments, it will attempt to use non-existent or incorrect registers (`r11+`). The calling convention requires overflow arguments to be passed on the stack.

### 22. [STABILITY] Hardcoded Stack Frame Size
`emit_prologue` hardcodes a 256-byte stack frame for every function:
```c
emit("    addi r29, r29, -256\n    stw r29, r31, 252\n    stw r29, r30, 248\n    addi r30, r29, 256\n");
```
This is wasteful for small functions and will cause a stack overflow for functions with more than ~240 bytes of local variables/saved arguments.

### 23. [INCOMPLETE] Comma Operator and `for` Loop Expressions
The `for` loop parser only supports a single expression in the initialization, condition, and increment sections. Standard C allows multiple expressions separated by the comma operator, which is frequently used in `for` loops.

### 24. [BUG] Global and Local Initialization Limitations
- Global variables are only allocated in `.bss` with `.space`; they cannot be initialized at declaration.
- Local variable initialization only works for scalar types (e.g., `int x = 5;`). Arrays cannot be initialized at declaration.

### 25. [STABILITY] Empty Error Reporting
The `cc_error` function is a stub.
```c
static void cc_error(const char *msg) {
    /* Minimal error handler — no stdio dependency. */
}
```
Providing no feedback on syntax or semantic errors makes debugging source code extremely difficult. Even a simple write to `stderr` or a halt would be preferable.

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
