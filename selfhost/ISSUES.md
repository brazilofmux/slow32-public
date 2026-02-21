# Code Review Issues: SLOW-32 Self-Hosting Toolchain

This document captures bugs, inconsistencies, limitations, and opportunities identified during review of `./selfhost` and adjacent toolchain components.

## Reading Guide

- **Section A**: Closed items (`[FIXED]`, `[RESOLVED]`, `[NOT SUPPORTED]` design decisions).
- **Section B**: Active correctness/conformance gaps.
- **Section C**: Missing language/ABI surface (feature backlog).
- **Section D**: Optimization backlog.
- **Section E**: Documentation/process follow-ups.

Note: Original issue numbers are preserved as recorded, including historical numbering collisions (for example, two separate Issue `#17` entries).

---

## Section A: Closed / Resolved / Decisioned

### Stage 0: Emulator (`s32-emu.c`)

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

### Stage 2: Forth Assembler (`asm.fth`)

### 6. [FIXED] `.byte` and `.word` Limited to Single Values
This was resolved in the v2 relocation (`selfhost/stage01/asm.fth`). `DO-BYTE` and `DO-WORD` now use a `BEGIN ... AGAIN` loop to process all tokens on a line.

### Stage 4: C Compiler (`cc.fth`)

### 8. [FIXED] String Truncation in Self-Host
This was resolved by the fix to Issue #6. The Stage 2 assembler now correctly parses comma-separated byte lists emitted by the C compiler.

### 11. [FIXED] Dead Code and Redundancy in `EMIT-LI-R2`
This was cleaned up during the v2 relocation. Both `EMIT-LI-R1` and `EMIT-LI-R2` now use identical, optimized logic without redundant temporary variables.

### Stage 4: C Compiler (`cc.fth`) — Long-Call Materialization / Array Size

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

### Bootstrap Flow / Runtime Artifacts

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

### Stage 8: Subset C Compiler (`cc-min-pass1.c`)

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

### 22. [FIXED] Hardcoded Stack Frame Size
`emit_prologue` now emits a placeholder that is back-patched after the function body is
parsed. `emit_prologue_final(frame_sz)` overwrites the placeholder with the actual frame
size, computed as `((-local_offset + 15) & ~15)` with a minimum of 32 bytes. This
eliminates the fixed 256-byte frame overhead and prevents stack overflow for functions
with many locals.

### 24. [RESOLVED] Global and Local Initialization Limitations
Global variables are only allocated in `.bss` with `.space`; they cannot be initialized
at declaration. Local array initialization is also unsupported. However, BSS zero-init
is sufficient for the selfhost toolchain, and all required initialization is done via
assignment statements.

### 25. [FIXED] Empty Error Reporting
`cc_error` now prints `cc-min:<line>: error: <msg>` to stderr using `fputs`/`fput_uint`.
Example output: `cc-min:5: error: unexpected character`.

### Recent Commit Review Follow-Ups

### 26. [RESOLVED] Missing `minizork.z3` Broke Zork Tests
Commit `def868a` removed `zork/stories/minizork.z3`, while `zork/tests/run-tests.sh`
requires that exact path. The story file has now been restored in commit `694ed6a`
(`Restore minizork story blob for zork tests`).

### 27. [FIXED] Stage02 cc-min Default Emulator Priority
`choose_default_emu()` in `run-cc-spike.sh` now prefers `slow32-dbt` before `slow32-dbg`.

### 28. [RESOLVED] Unused Locals in `cc-min-pass1.c`
`lsc` and `rsc` are declared and used in `emit_ptr_add()` and `emit_ptr_sub()`, not in
`parse_binop()`. No unused locals exist; this was a false positive.

### slow32-dbt

### 31. [FIXED] Infinite Loop on SSA-Compiled gen2.s32x

**Discovered**: 2026-02-20, during Phase C (register cache) development
**Pre-existing**: Yes — reproduces on pre-cache code (commit c2eef78)

slow32-dbt enters an infinite loop (100% CPU, zero progress) when running
gen2.s32x (the SSA-compiled s12cc compiler) on inputs that include ast.h.
slow32-fast completes the same workload correctly in under 5 seconds.

**Minimal reproduction:**

```bash
# Prerequisites: gen2.s32x built via standard bootstrap
#   gen1 = stage04 s12cc compiles stage05 s12cc.c → s12cc.s32x
#   gen2 = s12cc.s32x compiles s12cc.c → gen2.s → assemble → link → gen2.s32x

# Create minimal trigger (copy c_lexer_gen.c, pp.h, ast.h to same dir)
cat > /tmp/stub_ast.c << 'EOF_INNER'
#include "c_lexer_gen.c"
#include "pp.h"
#include "ast.h"
int main(int argc, char **argv) { return 0; }
EOF_INNER

# HANGS (infinite loop, 100% CPU):
slow32-dbt gen2.s32x /tmp/stub_ast.c /tmp/out.s

# WORKS (153M instructions, ~1 second):
slow32-fast gen2.s32x /tmp/stub_ast.c /tmp/out.s
```

**Threshold:**

| Test case | dbt | fast |
|-----------|-----|------|
| gen2 + lexer+pp (1617 lines, 133M inst) | **0.13s OK** | OK |
| gen2 + lexer+pp+ast (1989 lines, 153M inst) | **INFINITE LOOP** | ~1s OK |
| gen2 + s12cc.c (6692 lines, 518M inst) | INFINITE LOOP | 4.3s OK |
| gen2 + individual files (stdio, s32-as, s32-ld, etc.) | OK | OK |
| gen1 + s12cc.c (full self-compile) | OK (~seconds) | OK |
| SSA-compiled s32-as assembling 66K-line file | OK | OK |

**Stage-level bracketing** (corrected with signal-based probe):

| Stage | Behavior | Notes |
|-------|----------|-------|
| `-1` (no cache) | **WORKS** — makes progress | Was misidentified as "hang" — just very slow (retranslates every block) |
| `-2` (block cache) | **HANGS** at PC=0x0002CAA0 (`ssa_build_cfg`) | Block cache is the trigger |
| `-3` (+ inline lookup) | HANGS | Same root cause as stage 2 |
| `-4` (+ superblock) | **HANGS** at PC=0x00037038 (`hcg_phi_copies`) | Different function but same pattern |

Flags that do NOT help: `-R`, `-P`, `-S`, `-I`, and all combinations.
The bug is in the **block cache** (translate_block_cached), not the core
instruction dispatch or JIT optimizer.

**Analysis:**
- The sharp cliff between 133M (0.13s) and 153M (infinite loop) indicates
  a hard trigger, not gradual slowdown.
- gen1 (tree-walk-compiled, 184KB) compiles s12cc.c fine on dbt (all stages).
  gen2 (SSA-compiled, 268KB) hangs on stages 2+. Same source code, different
  machine code.
- SSA codegen patterns: spill-everything ldw/stw, PHI parallel copies
  (push/pop sequences), more basic blocks with branches.
- Small SSA-compiled programs run fine on dbt. Only gen2 at scale triggers it.
- Stage 2 and 4 hang at different PCs/functions, suggesting the cached block
  translations diverge from correct execution at different points depending
  on which blocks happen to be cached first.

**Narrowing — trigger pattern (lexer/pp base + calloc + struct arrow):**

The trigger requires BOTH a large code base (~1600 lines of lexer/pp) AND a
specific pattern. With lexer+pp as base code:

| Pattern | dbt |
|---------|-----|
| `sizeof(struct Node)` only | OK |
| `n->kind = kind` only (no alloc) | OK |
| `calloc(1, 76)` + raw cast `*((int*)n) = kind` | OK |
| `get_node()` returning `Node*` + `n->kind = kind` | OK |
| `calloc(1, sizeof(Node))` + `n->kind = kind` | **HANG** |
| `malloc(sizeof(Node))` + `n->kind = kind` | **HANG** |
| Two function calls without struct | OK |

Each piece alone works. `malloc/calloc + struct->member` combined hangs.
Raw pointer cast with the same calloc call works — only the struct `->` path
through the compiler triggers it.

Without the lexer/pp base code (~1600 lines), the same calloc+arrow pattern
works fine. The base code puts gen2 in a state where the pattern triggers.

**Critical test — gen1 vs gen2:**

```
gen1 (tree-walk compiled) + test_combo.c + dbt → OK
gen2 (SSA compiled)       + test_combo.c + dbt → HANG
```

Same compiler source, same emulator, same input. Only difference: gen2's
machine code was generated by the SSA backend. This suggests the SSA codegen
produces subtly incorrect machine code that manifests as divergent behavior
between emulators. slow32-fast may tolerate the bug (e.g., a wrong value that
doesn't affect control flow on fast's memory layout), while slow32-dbt diverges
into an infinite loop.

**Probe diagnostics** (added 2026-02-20):

Signal-based probe added to slow32-dbt (`-q` flag): SIGALRM fires every N
seconds and prints cpu->pc via async-signal-safe write(). This works even when
the JIT code never returns to the dispatch loop (the signal interrupts the
native code).

Stage 4 stuck PC: `0x00037038` in `hcg_phi_copies` (PHI elimination codegen).
The code at this address is `stw fp+-300, r1` — storing the return value of
`cg_s()` (string output). The enclosing loop pushes PHI argument values via
`hcg_into()` + `cg_s()`. This is a while loop that traverses PHI nodes.

Stage 2 stuck PC: `0x0002CAA0` in `ssa_build_cfg` (SSA CFG construction).
Different function than stage 4, suggesting the block cache corruption affects
different code paths depending on cache layout.

In both cases: SP/FP/LR are constant across all probes = the loop is tight,
never calling deeper or returning. r1=0 (stage 4) or r1=1 (stage 2).

**Hypotheses (updated with probe data):**

1. **Block cache hash collision / stale block**: The block cache (used in
   stages 2-4) may serve a stale or incorrect translated block for a guest
   PC due to a hash collision or invalidation bug. The cached block jumps
   to the wrong target, creating the loop. Stage 1 (no cache) always
   retranslates fresh, so it never hits this. **Leading theory.**

2. **Block chaining mislink**: When the dbt chains blocks (direct jump from
   one translated block to the next), a chaining patch may link to the wrong
   target after a cache eviction or when two blocks share the same hash slot.

3. **SSA codegen + block cache interaction**: The SSA backend produces many
   small basic blocks with frequent branches (more blocks than tree-walk
   codegen). This increases cache pressure and collision probability, making
   a latent cache bug manifest only at scale with SSA code.

4. ~~dbt instruction emulation bug~~ — ruled out. Stage 1 (same decode
   loop, no cache) works fine.

**Root cause found (2026-02-20):**

`cache_insert()` in block_cache.c used linear probing to find an empty hash
table slot but had **no guard against a full table**. The hash table had
`BLOCK_CACHE_SIZE=4096` slots, but the block pool allowed `MAX_BLOCKS=8192`
entries. When a program with >4096 unique basic blocks was run, `cache_insert`
looped forever scanning for a NULL slot that didn't exist.

The SSA codegen creates many small basic blocks (~5897 for s12cc), easily
exceeding 4096. The tree-walk codegen creates fewer, larger blocks, staying
under the limit. This is why gen1 worked but gen2 didn't — and why only
stages 2+ (which use the block cache) were affected.

**Fix:**
1. Added `cache_needs_flush()` — returns true when load factor exceeds 75%
2. Added pre-translation check in `translate_block_cached()` to flush before
   the hash table fills up
3. Bumped `BLOCK_CACHE_SIZE` 4K→128K and `MAX_BLOCKS` 8K→128K
4. Made `block_cache_t` static (BSS) to avoid 3MB+ stack allocation

**Verification:**
- 4K cache + guard: gen2 selfhost fixed point proven (gen2.s == gen3.s)
- 128K cache + guard: gen2 selfhost fixed point proven
- Full stage05 test suite: 22/22 pass

---

## Section B: Active Correctness / Conformance Gaps

### Stage 2: Forth Assembler (`asm.fth`)

### 7. [INCONSISTENCY] PC-Relative Reference Points
The reference point for PC-relative offsets differs between instructions:
- `JAL` (0x40): Relative to `PC`.
- `BEQ/BNE/...` (0x48+): Relative to `PC + 4`.
While the assembler compensates for this in `PARSE-TARGET` vs `PARSE-BTARGET`, it is an inconsistent design that complicates manual assembly and debugging.

### Stage 6: Subset C Archiver (`s32-ar.c`)

### 12. [NARROWED] Stage02 `s32-ar.c` Needs Full Command Coverage
The Stage02 subset archiver (`selfhost/stage01/validation/s32-ar.c`) now supports bounded multi-member create and real `rc` replace-on-existing behavior with basename matching.

Remaining gap:
- Expand command surface and parity checks for `d/m/v/p` paths before Stage02 can be considered complete.

### Stage 8: Subset C Compiler (`cc-min-pass1.c`)

### 21. [CC] Caller-Side Argument Limit
The compiler only supports up to 8 arguments in function calls, passed via registers `r3-r10`.
```c
while (k > 0) {
    k = k - 1;
    emit("    ldw r"); emit_num(3 + k); emit(", r29, 0\n    addi r29, r29, 4\n");
}
```
If a function is called with more than 8 arguments, it will attempt to use non-existent or incorrect registers (`r11+`). The calling convention requires overflow arguments to be passed on the stack.

### 23. [INCOMPLETE] Comma Operator and `for` Loop Expressions
The `for` loop parser only supports a single expression in the initialization, condition, and increment sections. Standard C allows multiple expressions separated by the comma operator, which is frequently used in `for` loops.

---

## Section C: Missing Language / ABI Surface (Feature Backlog)

### Stage 6: Subset C Compiler (`s12cc.c`)

### 33. [MISSING] `long long` (64-bit) Support
`is_type` recognizes `long`, but `parse_type` and the rest of the toolchain treat it as a 32-bit `TY_INT`. There is no support for true 64-bit integers.
**Recommendation**: Add `TY_LONG_LONG` and implement 64-bit lowering (pair of 32-bit registers/stack slots) and arithmetic/logical expansion.

### 34. [MISSING] Floating Point Support
`float` and `double` are completely missing.
**Recommendation**: Add type support and either implement soft-float library calls or MMIO-based FPU interface.

### 35. [LIMITATION] Preprocessor Gaps
`pp.h` lacks function-like macros, `#if` expression evaluation, `#undef`, and predefined macros like `__FILE__`/`__LINE__`.
**Recommendation**: Enhance `pp.h` to support token-based macro expansion and a more complete expression parser for `#if`.

### 38. [MISSING] `struct` by Value
Currently, structs can only be effectively accessed via pointers. Passing or returning a struct by value is not supported in the parser or calling convention.
**Recommendation**: Update calling convention and codegen to handle struct copies on stack/registers.

### 39. [LIMITATION] For-Loop Expression Limit
As noted in Issue #23, `for` loops only support single expressions in the init/cond/step sections. Standard C allows comma-separated lists.
**Recommendation**: Update `parse_for` in `parser.h` to handle comma-separated expression lists.

---

## Section D: Optimization Backlog

### Stage 6: Subset C Compiler (`s12cc.c`)

### 32. [OPPORTUNITY] `switch` Comparison Chain
`ND_SWITCH` is lowered in `hir_lower.h` to a linear chain of `HI_SEQ` + `HI_BRC`. For switches with many cases, this is O(N) at runtime.
**Recommendation**: Implement jump tables for dense case ranges and binary search for sparse ranges.

### 36. [OPPORTUNITY] Register Allocation: Caller-Saved Registers
`hir_regalloc.h` only uses callee-saved registers (`r11-r28`). This forces every function to save/restore registers in the prologue/epilogue even if it has very few locals or short live intervals.
**Recommendation**: Use `r3-r10` (caller-saved arguments) for short-lived values and arguments to minimize save/restore overhead.

### 37. [OPPORTUNITY] SSA Optimization: Memory SSA & GVN
Current SSA optimizations are mostly local or simple algebraic folds. Promotion to SSA is limited to scalar locals.
**Recommendation**: Implement Memory SSA to track memory state, allowing for global CSE across loads/stores and better dead store elimination. Implement Global Value Numbering (GVN) for more powerful redundancy removal.

### 40. [OPPORTUNITY] Tail Call Optimization (TCO)
Self-hosted compilers, especially those handling recursive descent (like `s12cc`), can benefit significantly from TCO.
**Recommendation**: Detect tail calls in `hir_opt.h` and rewrite to jumps to function start.

---

## Section E: Documentation and Process Follow-Ups

### Stage 4: C Compiler (`cc.fth`)

### 9. [DOC] Misleading Type Encoding Comments
Comments in `cc.fth` claim that `TY-STRUCT` indices and array counts overlap in the type word (bits 16-31), but the code actually uses disjoint ranges (14-21 for structs, 22-31 for arrays). The comments should be updated to reflect the actual (correct) implementation.

### Documentation Opportunities

### 10. `ISA-ENCODING.md` Errors and Discrepancies
- The "Operation" column for `SLTI` is a copy-paste error from `ADDI` (or rather, `SLTI` is missing from the I-Type table while appearing in other sections).
- Opcode ranges for R-type vs I-type are not clearly defined. Instructions `0x18` (SGT) through `0x1D` (SGEU) are R-type (using `rs2`) but reside in the `0x10-0x1F` range typically reserved for I-type arithmetic.
- **Major Discrepancy:** The instruction format diagrams show `funct7` (bits 25-31) and `f3` (bits 12-14) fields, but the Stage 0 emulator (`s32-emu.c`) and Forth assembler (`asm.fth`) use a flat 7-bit opcode space, effectively ignoring or zeroing these fields. This should be clarified to avoid confusion for developers familiar with standard RISC-V.

### 13. [DOC] Stale Implementation Status in `STAGE0-EMULATOR.md`
The `STAGE0-EMULATOR.md` file lists several instructions as "deferred" or "not needed by Forth kernel" (e.g., `DIV/REM`, `BLTU/BGEU`, `LDH/STH`, `MULH`), but these are already fully implemented in the actual `s32-emu.c` source. The documentation should be updated to reflect that the Stage 0 emulator is more capable than initially planned, while still maintaining the "intentional subset" philosophy (no floating-point, no 64-bit integers).

### 14. [DOC] Intentional Subset Alignment
While `./docs/INSTRUCTION-SET.md` defines a rich set including floating-point and 64-bit conversions, the `./selfhost` toolchain intentionally targets a minimal integer-only subset. This boundary is mostly respected, but the documentation in `selfhost/docs/` should more explicitly cross-reference the main ISA spec to clarify that self-hosting is a "reduced surface" effort.

### Recent Commit Review Follow-Ups

### 29. [DOC] Commit Message vs Touched Files Audit Trail
Commit `a851552` message says it fixes issues `#1/#2/#19/#25`, but touched files are:
- `selfhost/ISSUES.md`
- `selfhost/stage01/cc.fth`
- `selfhost/stage02/*`

No Stage0 emulator source file is touched in that commit. If Stage0 fixes were landed
earlier, the message should clarify that this commit updates tracking/docs for #1/#2
rather than containing those code changes directly.
