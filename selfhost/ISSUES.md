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

### 21. [FIXED] Caller-Side Argument Limit
Stage02 `cc-min-pass1.c` now handles calls with more than 8 arguments by:
- loading register arguments (`r3-r10`) from a temporary argument block
- re-packing overflow arguments to caller stack in ABI order (`arg9` at `sp+0`, `arg10` at `sp+4`, etc.)
- cleaning up caller stack space after call

Function prologues now also materialize parameters `9+` from caller stack (`fp+0`, `fp+4`, ...)
into local parameter slots, so both caller and callee sides agree for `>8` arguments.

### 23. [FIXED] Comma Operator and `for` Loop Expressions
Stage02 `cc-min-pass1.c` now parses comma-separated expression lists in all three `for`
clause slots (init/cond/step). This enables idioms like:
- `for (i = 0, j = 10; i < n, j > 0; i = i + 1, j = j - 1) { ... }`

Validation: targeted stage02 test returns expected value for mixed init/cond/step lists.

### Stage 13: Stage 07 (`selfhost/stage07/`)

### 31. [FIXED] stage07 fixed-point fails — regalloc bug from compare-and-branch fusion

**Status as of 2026-05-09**: `selfhost/stage07/run-tests.sh --fixed-point` passes
48/48, including the gen2 == gen3 byte-identical compare. The 2026-05-08 analysis
below correctly localized the surface symptom (gen1_cc miscompiles s12cc.c such
that `n` in `hl_stmt` has too short a live interval), but the root cause turned
out to be upstream of regalloc: parser local `{0}` initializers were leaving
unwritten fields uninitialized in locals (see commit `300f5833`, "zero-fill unset
fields in local {0} initializers"). Whichever struct inside the live-range path
relied on `{0}` was getting stale stack bytes, which then perturbed the live
interval scan. With `{0}` now zero-filling, the previously-corrupt path computes
`n`'s interval correctly and the spill survives.

The historical bisect notes are retained below for future reference — they
identify the *visible* miscompilation but not the actual upstream cause.

---

**Status as of 2026-05-08 (later)**: Codex's `bd04478f` lex_next dodge
has been reverted (`50b468f3`) so the symptom now surfaces honestly:
gen1 builds, fp-gen2 builds, but `fp-gen2.s32x` errors out with
`s12cc:176: expected token 51 got 0` (rc=10) when asked to compile
`s12cc.c` for fp-gen3 — the gen3 compile never produces output, so
the gen2==gen3 byte comparison is never reached.  The bug is **not**
"gen2 ≠ gen3 by N lines"; it's "gen2's compiler is internally
miscompiled and can't lex its own source."

**Diagnostic harness landed in `68dd34a6`**: `selfhost/stage07/diff-corpus.sh`
+ a 10-file corpus reproduces the smoking gun in ~1.5 s.  Smallest
signal: `01_return_const.c` (`int main(void){return 0;}`) — fp-gen2's
output drops the `addi r1, r0, 0` that `cc.s32x` and `gen1_cc.s32x`
both emit (they're byte-identical for this input).  The bug is purely
in fp-gen2 (gen1_cc's compile of s12cc.c miscompiles some function
inside s12cc.c that fp-gen2 then runs incorrectly).

**Don't-NOP probe (2026-05-08)**: patched `ra_extend_fused_cmp` to
keep the live-range extension but skip the `h_kind[cmp] = HI_NOP`
and intermediate-COPY-NOP lines.  Re-ran `--fixed-point`: same
symptom, same `expected token 51 got 0`, same 10/10 DIFFER on the
corpus, same 10-line missing-`addi r1, r0, 0` on `01_return_const`.
**The bug is not in the NOPing logic.**  Combined with the earlier
"force `hcg_identify_fusions` to early-return" experiment also
failing to restore the gate, this confirms ISSUES.md's hint:
**something between `7b9e1405` and HEAD compounds with (or
independently triggers) the same surface.**

Next bisect target: between `7b9e1405` (the fusion commit, where
the symptom first surfaces) and `be1514b4` (last commit to rebuild
the checked-in `cc.s32x` reference).  Likely suspects in that range
that touch HIR/regalloc/codegen:
- `e92607d4` HIR-codegen global-init byte/element semantics
- `f5cde4d3` register compiler-generated temp allocas with hl_ainst
- `83fc86ab` mem2reg false-rejection for allocas + verifier
- `3b865b34` hir_opt: promote single-store allocas (trivial mem2reg)

**Bisect attempt (2026-05-08, third)**: ran
`git bisect run /tmp/bisect-step.sh --disable-fusion` over the range
`cc06e0a98..df24ca02` (54 commits). First-bad: **`874483ff`** "Extend
stage07 aggregate init and PP support". But the failure mode at
`874483ff` is *different* from HEAD's symptom:

| commit | mode | observation |
| --- | --- | --- |
| `cc06e0a98` (good) | — | fp-gen2 builds, `addi r1, r0, 0` present |
| `874483ff` (first-bad) | gen1_cc parser crash | `s12cc:138: expected token 54 got 1` building fp-gen2 |
| `e92607d4` | gen1_cc PHI overflow | `s12cc: too many phi args` building fp-gen2 |
| `fafd57f1` (HIR_MAX_PARG bump) | emission bug | fp-gen2 builds, **but missing `addi r1, r0, 0`** |
| `df24ca02` (HEAD) | emission bug | same as fafd57f1 |

So `874483ff` is the first commit where the bisect script's BAD
predicate fires, but several distinct regressions are stacked in
this range:
1. **`874483ff`** changed `ps_ginit_count` semantics (element-count
   → byte-count) and `ps_ginit_pool` (element-stride → one byte per
   slot).  It updated `codegen.h:gen_data` but missed
   `hir_codegen.h:gen_data` — fixed at `e92607d4`.  However, the
   commit also touched `parser.h` (+293 lines) and `pp.h` (+94
   lines); the gen1_cc parser-crash *might* live in those rather
   than the byte-semantic miss.
2. **`e92607d4`** fixed the byte/element issue in `hir_codegen.h`
   but was masked by a `HIR_MAX_PARG=8192` overflow when self-compiling.
3. **`fafd57f1`** bumped `HIR_MAX_PARG` to 32768, unmasking the
   *emission bug* — the gen1_cc miscompilation of s12cc.c that drops
   `addi r1, r0, 0` from fp-gen2's `main`.

The emission bug is therefore *not* introduced at `fafd57f1` — that
commit is just numeric.  It's latent in earlier code (probably
something in the `874483ff..e92607d4` range or earlier still) and
only became observable once gen1_cc could finish self-compilation.

**Why the coarse bisect can't find it**: the BAD predicate
("fp-gen2 not built OR fp-gen2 missing addi") fires for both crash
and emission failures.  Across `874483ff..e92607d4`, gen1_cc cannot
build fp-gen2 (different reasons — parser limits, phi overflow), so
the emission bug is unobservable.

**Next angle**: instead of bisecting, do a *differential code path*
analysis on gen1_cc compiling some single function from s12cc.c.
- Identify which function (or functions) gen1_cc miscompiles by
  diffing fp-gen2.s against the cc.s32x-built reference.
- The session log noted `hl_stmt` lost a parameter spill — that's a
  good first target.  Ditto for whichever function emits `addi
  rN, r0, 0` for `return 0;` (likely `cg_iconst` or `gen_return`
  inside `s12cc.c` — find by grep).
- With the suspect function isolated, build a minimal C program
  that exercises only that function's pattern, and compare gen1_cc's
  output to cc.s32x's.  If gen1_cc miscompiles even a stripped-down
  version, you have a reproducer that doesn't need self-hosting.

**Reusable helper**: `/tmp/bisect-step.sh` (and the in-repo
`selfhost/stage07/bisect-step.sh` it was copied from at this commit)
is now `BISECT_ROOT`-aware so it survives `git checkout` to commits
that don't have it on disk.  Updated comment block documents the
crash-vs-emit caveat.

**Localization (2026-05-08, post-bisect)**: differential analysis on
`hl_stmt` confirms the original session's diagnosis verbatim — the
bug is in regalloc, not in the byte/element mismatch from `874483ff`.

Side-by-side dump of `hl_stmt`'s prologue, both compiles of the
identical `s12cc.c@HEAD`:

```
# cc.s32x compiled (REFERENCE — the spill survives)        # gen1_cc compiled (fp-gen2 — n is clobbered)
hl_stmt:                                                   hl_stmt:
    addi r29, r29, -216                                        addi r29, r29, -204
    ... callee-saves ...                                       ... callee-saves ...
.L8171:                                                    .L8171:
    addi r11, r3, 0          # r11 = n                         addi r11, r3, 0          # r11 = n
    stw r30, r11, -12        # SPILL n -> fp-12                seq r12, r11, r0         # !n test (no spill emitted)
    ldw r11, r30, -12        # reload                          ...
.L8174:                                                    .L8176:
    ldw r16, r30, -12        # n always reloaded              lui r1, %hi(hl_struct_ret)
    addi r15, r16, 52                                          ldw r17, r1, %lo(hl_struct_ret)
                                                               xori r11, r17, 0         # r11 CLOBBERED — was n
                                                               sne r11, r11, r0
```

The reference codegen spills `n` to fp-12 immediately and reloads on
every use, so the `&&`-chain intermediate (the `xori …` materializing
`hl_struct_ret`) freely overwrites r11 without losing `n`.  fp-gen2
keeps `n` only in r11, then assigns `xori`'s result to r11 inside the
&&-chain, clobbering `n`.  When the `else if (n->lhs)` arm runs, it
reads `n->lhs` from a corrupt r11 — sees 0 — falls through to the
no-value `else` arm and emits `HI_RET, src1=-1`, producing fp-gen2's
empty `main` (no `addi r1, r0, 0`).

So the `iconst_use: total=0` symptom is not lowering dropping the
ICONST — it's the ND_RETURN path emitting a *void* RET because the
n-corruption sends control down the wrong if/else arm.

**The miscompilation is gen1_cc's compilation of hir_regalloc.h**
(or a function it calls).  Specifically, gen1_cc must miscompile
either `ra_compute_ends` (live-range scan + `ra_extend` + cross-block
`ra_backprop`) or `ra_linear_scan`, in such a way that `n`'s live
interval is *too short* — short enough that linear-scan releases
r11 mid-function and reuses it for the &&-chain intermediate.  cc.s32x's
copy of the same source computes the long interval correctly.

**Why fusion-disable didn't help**: `ra_extend_fused_cmp` reads
`hcg_brc_fuse[]` and is a real no-op when the array is empty (the
fusion-disable patch confirmed via `BISECT-INFO` that it cleared the
array).  But the bug isn't in `ra_extend_fused_cmp`; it's in the
*regular* live-range path that runs unconditionally.  The fact that
`hl_stmt` has no fused compares in this trace (line `seq r12, r11,
r0; beq r12, r0, .L8292` — separate seq+beq, not a fused beq) makes
this concrete: fusion isn't even firing here, yet the bug is.

**Next angle**: instead of bisecting compiler commits, instrument
gen1_cc and the reference cc.s32x to dump live-interval `[start..end]`
for each value in `hl_stmt`.  Diff the dumps; the values whose
intervals differ are the ones whose regalloc decision changed.
Then walk back to whichever ra_* function computed those intervals —
if `ra_extend` saw fewer uses in fp-gen2, the bug is in the source-of-uses
scan (likely how operand references are walked); if `ra_extend` saw
the same uses but `ra_iend` ended up smaller, the bug is in `ra_extend`
itself.

A `--dump-intervals` flag on s12cc would localize this in one run
without re-bisecting.

Alternative low-cost workaround: emit an unconditional spill+reload of
every parameter at function entry (matching what cc.s32x's codegen
does today).  This makes the regalloc bug invisible because params
always go through stack.  Cost: one stw + one ldw per param at entry,
plus one ldw per use.  Acceptable as a stop-gap if root-causing the
regalloc bug stays expensive.

**Caveat about the cc.s32x reference**: `selfhost/stage07/cc.s32x`
is a checked-in binary last rebuilt at `be1514b4`.  Many commits
since then have changed `s12cc.c` and the compiler internals, so
cc.s32x is an *older version* of the compiler — not a fresh
host-LLVM build.  It's still useful as a reference (it predates
the bug), but it disagrees with `gen1_cc.s32x` on more complex
corpus inputs (5/10 in our corpus) for benign reasons (different
versions of the same source), not the gen2 miscompilation.

---

**Original session notes follow.**

Codex's commit `bd04478f` adds a hand-written
fast path in `lex_next` (whitespace + string/char literals) so gen2 now
**builds** — the visible "expected token 56 got 0" error from the prior
session is gone.  The fixed-point gate still fails (47/48): gen2 ≠ gen3.
The Codex change is a workaround for the symptom, not the root-cause fix.

**Root cause (bisected)**: `git bisect` between `ce960027` (good, 48/48)
and `bd04478f` (bad, 47/48) using `selfhost/stage07/run-tests.sh
--fixed-point` lands on:

```
7b9e1405 Add compare-and-branch fusion and fallthrough elimination to s12cc
```

The commit added:
- `hir_burg.h:hcg_identify_fusions` — marks BRC instructions whose
  condition is a single-use comparison, and their intermediate COPYs.
- `hir_regalloc.h:ra_extend_fused_cmp` — extends the fused comparison's
  operand live ranges to the BRC position, then NOPs the comparison
  (and intermediate COPYs).
- `hir_codegen.h` — direct beq/bne/blt/... emission for fused BRCs.

The live-range extension + NOP-rewrite has a soundness bug: many later
functions stop spilling parameters/locals.  Visible symptoms in
`fp-gen2.s` and `fp-gen3.s`:
- `hl_stmt`: `n` (the `Node *` parameter) is no longer spilled at
  `fp-12`.  Without the spill, `n` is reused-then-clobbered for the
  `n->lhs && hl_struct_ret && ty_is_struct(...)` short-circuit chain,
  and the `else if (n->lhs)` path reads `*(0+52)` instead of
  `*(n+52)`.
- `int main(void) { return 0; }` (and many other simple programs):
  the `addi r1, r0, 0` constant load for the return value is missing.
  Stats show `hir_burg_select: total=1` and `hir_iconst_use: total=0`
  where cc.s32x (host-LLVM build, same source) reports `total=2` /
  `total=1`.  Same source, different runtime regalloc decisions.

**Confirmation experiment**: forcing `hcg_identify_fusions` to early-return
(no candidates marked, so `ra_extend_fused_cmp` is a no-op and codegen
falls back to the unfused BRC path) does **not** restore the gate at
HEAD.  Reading: subsequent commits between `7b9e1405` and HEAD compound
with the original bug, or rely on fusion working correctly.  The clean
fix is to make `ra_extend_fused_cmp` sound rather than to disable it.

**Files changed in 7b9e1405** (start here):
- `selfhost/stage07/hir_burg.h` (`hcg_identify_fusions`, +81 lines)
- `selfhost/stage07/hir_regalloc.h` (`ra_extend_fused_cmp`, +53 lines)
- `selfhost/stage07/hir_codegen.h` (HI_BRC fusion-aware emission, +134 lines)

**What's likely wrong**: `ra_extend_fused_cmp` runs after
`ra_compute_ends` but before `ra_linear_scan`.  Extending the
comparison's *operand* live ranges to the BRC position is necessary,
but the comparison itself becoming HI_NOP changes the live-range layout
that `ra_compute_ends` already produced for everything that surrounded
it.  Likely candidates:
- The comparison's own value (the SEQ/SNE/... result) may still appear
  as a use somewhere `ra_compute_ends` recorded but
  `ra_extend_fused_cmp` doesn't account for.
- Intermediate-COPY NOPing changes the dependency chain from the
  comparison to the BRC — anything downstream computed from the
  pre-NOP state of `bg_uses` / `ra_pos` / live ends becomes stale.
- `ra_pos[BRC]` is read for `brc_pos` but other instructions' live
  ranges that incidentally cross the now-NOPed comparison may not be
  re-extended even though they should be.

**Reproducer**:

```bash
selfhost/stage07/run-tests.sh --emu tools/dbt/slow32-dbt --fixed-point \
    --keep-artifacts
# → FAIL: stage07 (47/48 tests passed, 1 failed)
WD=$(ls -dt /tmp/selfhost-v2-stage07.* | head -1)
diff $WD/fp-gen2.s $WD/fp-gen3.s | head
# → many `< addi r1, r0, X` lines (gen3 missing constants gen2 has)
```

Smaller cross-check: any host-built `cc.s32x` and stage06-built
`gen1_cc.s32x` produce identical output for `int main(void) { return 0; }`,
but `fp-gen2.s32x` (gen1's compile of s12cc.c) drops the `addi r1, r0, 0`.

**Bisect command (preserved for re-run)**: see
`/tmp/bisect-helper/run.sh` from the 2026-05-08 session — runs
`run-tests.sh --fixed-point` with a 600 s timeout and grepping
`fixed-point:` for PASS/FAIL.

### Cross-Emulator Sum Verification (2026-05-09)

A full sweep on the Lenovo ARM box rebuilt stages 01..07 under each of
`slow32`, `slow32-fast`, `slow32-dbt`, and the cc-a64-cross-compiled
`dbt-a64`, comparing SHA256 of every output `.s32x` against
`selfhost/sha256sums.md`. Only `slow32-dbt` passed end-to-end. The other
three exposed independent bugs documented below as #41/#42/#43. Verifier
script committed at `selfhost/verify-emu-sums.sh`; per-emulator logs at
`/tmp/verify-emu-sums-<label>/`.

Status as of 2026-05-09 (later): #41 fixed (slow32 getopt). #42 fixed
(slow32-fast cap removed). #43 partial (3 MMIO ops added to dbt-a64
cross libc); #44 opened for the remaining s32-as.s32x startup hang.

**`s32fast-hir` (cc-x64 cross-compiled emulator) sweep — done.**
Stage 7 artifacts produced under `selfhost/stage07-cross/out/s32fast-hir`
on AWS Xeon Platinum 8259CL match the canonical Stage 7 SHA-256s; row
added to `selfhost/sha256sums.md` in commit `30fef05e`. No outstanding
work on the s32fast-hir verification path — future sessions should not
re-run this unless the codegen or libc changes invalidate the hashes.

### 41. [FIXED] `slow32` getopt permutation consumes program flags

**Status**: fixed 2026-05-09 in `tools/emulator/slow32.c:1096` by prefixing
the optstring with `+` (`"+hstrc:b:w:"`), which switches getopt to
POSIX-style first-non-option-stops parsing. Verified by re-running the
verifier on stage02: build under `slow32` now succeeds and produces
bit-identical artifacts (s32-as, s32-ar, s32-ld, cc-min) in 99 s.

**Original symptom**: rebuilding stages with
`SELFHOST_EMU=tools/emulator/slow32` failed at stage02 cc-min link with
cascading `invalid option -- '-'` / `-- 'm'` / `-- 'o'` errors, then
`error: cannot open .../cc-min.s32x` because the linker's
`-o cc-min.s32x` was eaten as host-side options and the output path was
reinterpreted as the s32x to load.

**Root cause**: getopt's default GNU mode *permutes* `argv` to scan past
positional arguments looking for more options — so any `-X`/`--X` token
meant for the guest s32x program (here: `--mmio 64K` and
`-o cc-min.s32x` passed to `s32-ld.s32x`) was consumed by the host loop.
`slow32-fast` (`slow32-fast.c:1513+`) and `slow32-dbt` parse argv
manually and forward unknown flags to the guest, which is why they
don't hit this.

**Reproducer (pre-fix)**:
```bash
cd selfhost/stage02 && make clean
SELFHOST_EMU=$PWD/../../tools/emulator/slow32 make 2>&1 | tail -20
```

### 42. [FIXED] `slow32-fast` produces empty `.s32o` for stage05 Phase-1 `s12cc.s`

**Status**: fixed 2026-05-09 in `tools/emulator/slow32-fast.c`. Default
instruction cap was hard-coded to `10000000000` ("10B instruction limit
for debugging") with no flag to override; assembling stage05's
2.2 MB `s12cc.s` consumes ~32 billion instructions, so the assembler
hit the cap and exited with `rc=24` (1054488 mod 256) before writing
any output. Resolution: default to unlimited (`max_instructions == 0`),
add a `-c <N>` flag to opt back into a cap (matching slow32's
convention). Verified: `s12cc.s32o` produced under fixed slow32-fast is
bit-identical to the slow32-dbt reference (130 s, 32 B instructions).

**Original symptom**: with `SELFHOST_EMU=tools/emulator/slow32-fast`,
stages 01..04 produced bit-identical artifacts but stage05 failed
during `[3/4] Compile compiler` with
`assemble produced no output: /tmp/stage05-build.XXX/s12cc.s`. The
assembler ran for ~40 s, printed
`WARNING: Execution limit reached (10000000000 instructions)`, then
emitted a 0-byte `.s32o`. The build script's exit-code check
(`rc != 0 && rc != 96`) caught the failure but the message
("assemble produced no output") obscured the real cause.

**Reproducer (pre-fix)**:
```bash
cd selfhost/stage05 && make clean
SELFHOST_EMU=$PWD/../../tools/emulator/slow32-fast \
  SELFHOST_TIMEOUT=2400 make 2>&1 | tail -20
# WARNING: Execution limit reached (10000000000 instructions)
# assemble produced no output: /tmp/stage05-build.XXX/s12cc.s
```

### 43. [PARTIAL] `dbt-a64` (cc-a64 cross-compiled DBT) MMIO gap chain

**Status**: three MMIO operations added to `libc_a64/mmio_ring_a64.c`
on 2026-05-09 — root-cause was a deliberately stripped MMIO ring stub,
not a syscall bug. Each fix uncovered the next layer.  See #44 for
the remaining issue (s32-as.s32x hangs at startup under dbt-a64).

**Layer 1 — STAT [FIXED]**: original symptom on Forth kernel +
selfhost stage02 was `Error line 0 : cannot stat input file`. Cross
libc's stub returned `S32_MMIO_STATUS_ERR` for STAT. Added handler
that calls `stat`/`fstat` (via newfstatat / fstat syscalls) and
repacks the AArch64 asm-generic `struct stat` (128 bytes) into the
`s32_mmio_stat_result_t` layout.

**Layer 2 — OPEN flag translation [FIXED]**: stat succeeded, then
got `Error line 0 : read error`. OPEN handler was passing
`req->status` (SLOW-32 flags: 0x01 read, 0x02 write, 0x04 append,
0x08 create, 0x10 trunc) directly to Linux `openat` as the flags
arg. SLOW-32 read=0x01 collided with Linux `O_WRONLY=1`, so files
opened "for read" were actually write-only. Added flag translation.

**Layer 3 — SEEK [FIXED]**: open/read worked, but the linker (Forth
+ link.fth, used to link libc archive) failed at `Loading archive:
libc.s32a / Error: cannot read archive header`. Cross libc had SEEK
in its stripped-out set. `AR-READ-AT` in `selfhost/stage01/ar.fth`
calls `REPOSITION-FILE` (Forth seek) before each archive read, so
without SEEK the linker silently dropped libc and emitted hundreds
of `Error: unresolved reloc symbol: strcmp/strtol/strncmp/strchr`
lines, then wrote a malformed `s32-as.s32x`. Added SEEK handler that
calls `lseek`. Bisect was clean: per-file diff against the gcc-dbt
workdir showed all `.s`/`.s32o`/`.s32a` outputs byte-identical except
`link.log` (gcc=6390 bytes vs a64=21103 bytes).

**Cumulative effect**: dbt-a64 now produces canonical s32-as.s32x
(83832 bytes, sha 065f0dc9…) on stage02 step 1. Stage01 already
passed; later stages still fail because of a separate hang documented
as #44.

**Reproducer (now passes)**:
```bash
cat forth/prelude.fth selfhost/stage01/asm.fth - <<'FTH' \
  | timeout 60 selfhost/stage07-cross-a64/out/dbt-a64 forth/kernel.s32x
S" /absolute/path/to/selfhost/stage02/crt0.s" S" /tmp/crt0.s32o" ASSEMBLE
BYE
FTH
# rc=96, no error, /tmp/crt0.s32o is 356 bytes (text=48 data=0 bss=0 syms=2)
```

### 44. [OPEN] `dbt-a64` hangs at startup running `stage02/s32-as.s32x`

**Status**: open as of 2026-05-09. Surfaces after #43 layers 1-3 are
fixed.

**Symptom**: small s32x programs run fine under dbt-a64
(hello_minimal.s32x, test1.s32x, cc-min.s32x all complete cleanly).
But `selfhost/stage02/s32-as.s32x` (the canonical 83832-byte C
assembler) hangs at startup — even invoked with no args, it never
prints "Usage:", `dbt-a64 -v` shows the load info but no further
output. Same binary under `tools/dbt/slow32-dbt` runs the no-args
path in 28 ms. Effect: stage02 builds s32-as.s32x correctly, but the
freshly-built C assembler can't be used to bootstrap s32-ar.s32x.

**Why it matters**: blocks dbt-a64 from completing the selfhost
sweep. Stage01 verifies but stage02 fails at step 2.

**Suspected cause**: not an MMIO gap (s32-as.s32x doesn't print
anything before main, so the hang is before any I/O syscall). More
likely a dbt-a64 codegen bug — some SLOW-32 instruction or block
shape that only this larger binary exercises. The translator is
lazy; the failure could be in a translate-to-a64 path that rarely
fires. Worth running with `-v -s` to see whether translation events
stop, and instrumenting `dbt-a64`'s own translator to log the last
SLOW-32 PC before the hang.

**Reproducer**:
```bash
timeout 5 selfhost/stage07-cross-a64/out/dbt-a64 -v \
  selfhost/stage02/s32-as.s32x 2>&1 | head -10
# Loaded: ...s32-as.s32x
#   Code limit:  0x00100000
#   ...
# (then nothing — hangs until SIGTERM)
#
# Reference (works in 28 ms):
timeout 5 tools/dbt/slow32-dbt selfhost/stage02/s32-as.s32x
# → "Usage: s32-as <input.s> <output.s32o>"
```

### 45. [FIXED] `dbt-x64` (cc-x64 cross-compiled) stage-4 reg-cache hang

**Status**: fixed by `a9e65701` ("Fix HIR lowering after terminators",
2026-05-08), verified 2026-05-09. Freshly rebuilding `out/dbt-x64`
under the current cc-x64 and running `./out/dbt-x64
../stage01/hello_minimal.s32x` (default flags = stage 4 + reg cache)
prints `!` and exits 0; previously this configuration spun in a CPU-
bound infinite loop. All other configurations (`-2`, `-3`, `-4 -R`,
`--paranoid`) also pass.

**Root cause**: `hir_lower.h`'s block walker kept lowering non-label
statements after an unconditional terminator (`goto`/`return`/`break`/
`continue`). Ragel-generated state machines emit unreachable
statements after `goto _out`, so cc-x64 produced bogus HIR for
`tools/dbt/translate.c` etc., which only manifested as runaway loops
once the stage-4 register cache extended liveness through back-edge
prewarms. Fix: in `hl_stmt`'s ND_BLOCK loop, skip statements whose
kind is not `ND_LABEL`/`ND_CASE`/`ND_DEFAULT` while
`hl_terminated()` is true (`selfhost/stage07-cross/hir_lower.h:2048`).

**Reproducer (now passes)**:
```bash
cd selfhost/stage07-cross && make dbt
./out/dbt-x64 ../stage01/hello_minimal.s32x  # → "!", exit 0
```

### Verifier script (`verify-emu-sums.sh`)

Reusable harness for the above. Per-emulator: `make clean && make` each
stage, sha256sum every `*.s32x`, compare against `sha256sums.md`. Caveat:
`.s32x` files are gitignored, so a cascaded build failure leaves the
tree partial — don't rely on `git checkout` to restore. Either back up
artifacts before `make clean` or rebuild with a known-good emulator
afterwards. Live copy: `/tmp/verify-emu-sums.sh` (Lenovo box, 2026-05-09).

### 46. [LIMITATION] stage07 / cc-x64 stack-frame and recursion caveats

**Status**: open / awareness item, 2026-05-09. Not pressing — current
selfhost workloads (s12cc compiling itself, dbt-x64 compiling tools/dbt/)
fit comfortably. Captured here because larger third-party C codebases
(TCC, Lua, etc.) will likely brush against these limits.

**Frame-size scaffolding**: SLOW-32's `addi` immediate is 12-bit signed
(±2047). Stage07 codegen emits `lui+add` scaffolding for prologue /
epilogue / spill-slot offsets above 2047. Confirmed working, but it is
the path that historically misbehaves first when a regression bloats
frames. Watch points:
- `selfhost/stage07/hir_codegen.h` prologue/epilogue offset emission
- spill-slot allocation in `selfhost/stage07/hir_regalloc.h`

**Spill heuristics × frame size**: an earlier regalloc bug spilled
nearly everything, multiplying frame sizes 3–5× (`parse_stmt` went from
592 → 1968 bytes). The deep recursion in s12cc's parser then exhausted
the 256 KB heap-allocated user stack. The bug is fixed, but the
combination is fragile: any change that increases spilling on a
recursive function can re-introduce a "hang" that's actually a stack
overflow. `slow32-dbt` historically masked this (silent corruption);
`slow32-fast` catches it as a clean crash — prefer the latter when
chasing recursion-depth issues.

**256 KB user stack**: the runtime allocates a fixed 256 KB stack at
load time. Deep recursion (TCC's parser, Lua's parser, anything with
heavy AST nesting) can hit it. Recovery: bump the runtime stack size
(linker `--stack-size`) or refactor the offending pass to be iterative.

**Output buffer**: `CG_MAX_OUT` was bumped to 4 MB (stage04) to unblock
stage05; stage07 inherits this. Programs that produce > 4 MB of asm in
a single TU will silently truncate. Not a current concern but worth
noting.

**Not in scope here**: the hardware ISA's 12-bit immediate isn't going
to change. The work item, if/when forced, is making the regalloc spill
less aggressively and/or reducing parser recursion depth in s12cc
itself.

### 48. [WORKAROUND] cc-a64 regalloc clobbers `r_status` in mmio_process SEEK path

**Status**: workaround landed in `s32-fast-x64.c:OP_MMIO_SEEK`
(`fflush(0)` after the assignment); root cause is in cc-a64's regalloc.
Discovered 2026-05-09 while sweeping cc-a64-cross-compiled `s32fast-hir`
across stages 1-7 (stage02 step 2 hung running stage02's freshly-built
`s32-as.s32x`).

**Symptom**: under cc-a64-built `s32fast-hir` only, stage02 `s32-as.s32x`
goes into an infinite loop in the alignment-pad write at
`s32-as.c:909` (`while (((uint32_t)ftell(g_out) & 3u) != 0) fputc(0,
g_out)`). The output file grows unbounded (~250KB-7MB) with valid
header bytes followed by an infinite zero-fill. gcc-built `slow32-fast`,
gcc-built `slow32-dbt`, and `tools/emulator/slow32` all run the same
`s32-as.s32x` correctly.

**Root cause** (verified by host-side debug printfs in `mmio_process`):
the OP_MMIO_SEEK handler computes `r_status = (unsigned int)pos`
correctly (`pos` is the lseek return, e.g. 0x68 = 104), but the value
the host writes to `ra+12` (`mem_write32(ra+12, r_status, ...)`) is
`0x1` — the value of `next_head = (resp_head + 1) % MMIO_RING_ENTRIES`
on the first iteration. cc-a64's regalloc is reusing `r_status`'s
register/slot for `next_head` despite `r_status` still being live
across the long opcode-dispatch chain that separates the SEEK
assignment from the response-write block.

The bug shape matches `feedback_a64_rpo_walk.md` (commit `e743c098`,
ssa_rpo[i] vs ssa_rpo_ord[i]) — both are SSA/regalloc bugs in cc-a64
that only surface with long, branchy functions. `mmio_process` is one
of the longest in `s32-fast-x64.c`.

**Workaround**: a single `fflush(0)` after `r_status = (unsigned int)pos`
forces cc-a64 to spill the value to stack, sidestepping the regalloc
bug. Verified byte-identical output for both small (108-byte) and
large (356-byte+) assembler outputs.

**Real fix**: needed in cc-a64. Likely candidates: live-range
computation across the multi-arm `else if` chain that constitutes the
opcode dispatcher; cross-block use tracking after a value is set
inside a deeply-nested if/else.

**Reproducer**:
```bash
cd ~/slow-32 && cd selfhost/stage07-cross-a64 && make s32fast
cd ~/slow-32 && rm -f /tmp/min.s32o
echo -e '.text\n.global _start\n_start:\n    halt' > /tmp/min.s
timeout 1 selfhost/stage07-cross-a64/out/s32fast-hir \
    selfhost/stage02/s32-as.s32x /tmp/min.s /tmp/min.s32o
# Without the fflush(0) workaround: rc=124 (timeout), output grows
#   to >100KB.  With workaround: rc=0, output is byte-identical to
#   slow32-dbt's 108-byte output.
```

### 49. [OPEN] `dbt-x64` (cc-x64 cross-compiled) miscompiles `forth/kernel.s32x` on stage02 inputs

**Status**: open as of 2026-05-14. Surfaces after #45 was fixed
(a9e65701, "Fix HIR lowering after terminators"). `dbt-x64` passes
stage01 of `selfhost/verify-emu-sums.sh` byte-identically — including
running `forth/kernel.s32x` under stage01's smaller `asm.fth`
workloads — but fails immediately at stage02 step `[1/4] Assemble
runtime`.

**Symptom**: assembling `selfhost/stage02/crt0.s` via
`forth/kernel.s32x + stage01/asm.fth` through `dbt-x64` segfaults the
host (rc=139). gcc-built `slow32-dbt` returns rc=96 (Forth BYE) and
writes a 356-byte object file on the same input. Stage01's
`crt0_minimal.s` — 36 lines vs stage02's 34, ~95% identical aside
from comment text — does **not** trigger the bug. So the trigger is
content-specific in the lexer path of `asm.fth`, not program size.

**Two distinct failure modes on the same input**:

| dbt-x64 mode | guest input | rc | output |
| --- | --- | --- | --- |
| default (stage 4 + reg cache) | stage02/crt0.s | **139 (SIGSEGV)** | none |
| `-2` (block dispatch only)    | stage02/crt0.s | 96 (BYE) | **none — silent miscompile** |
| default                       | stage01/crt0_minimal.s | 96 (BYE) | 356 bytes ✓ |
| gcc `slow32-dbt`              | stage02/crt0.s | 96 (BYE) | 356 bytes ✓ |

The `-2` silent-miscompile (BYE reached but the FORTH `ASSEMBLE` word
produced no file) points to a JIT-translator codegen bug, not a host
memory error; the stage 4 SIGSEGV likely follows the same bad
translation once superblocks / reg cache extend liveness through it.

**Comparison to #44**: `dbt-a64` hangs at startup on a much larger
guest (`stage02/s32-as.s32x`, 83KB) with no I/O signal — `dbt-x64`
gets further (full stage01 runs, including the Forth kernel) and
gives an actionable reproducer.

**Reproducer**:
```bash
cd ~/slow-32 && cd selfhost/stage07-cross && make dbt && cd ../..
WORK=$(mktemp -d)
cat forth/prelude.fth selfhost/stage01/asm.fth - <<FTH \
    | timeout 60 selfhost/stage07-cross/out/dbt-x64 forth/kernel.s32x \
        > "$WORK/log" 2>&1
S" selfhost/stage02/crt0.s" S" $WORK/crt0.s32o" ASSEMBLE
BYE
FTH
# default mode: rc=139 (SIGSEGV), no output
# add `-2` after `dbt-x64`: rc=96, still no output (silent miscompile)
# gcc-built slow32-dbt on the same input: rc=96, 356-byte output
```

**Next-step suggestions**:
- Run with `dbt-x64 -2 --paranoid` to localise the first SLOW-32 PC
  where shadow-interp diverges from the JIT — that's the
  miscompiled block.
- Once a guest PC is known, `slow32dis forth/kernel.s32x <pc> <pc+N>`
  identifies the SLOW-32 instruction or block shape the translator
  is botching.
- Likely candidates: an instruction pattern that fires in the lexer's
  character-class table but not in the simpler stage01 path; or a
  superblock-extension fold that's incorrect under cc-x64's regalloc
  but correct under gcc's.

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

### 39. [RESOLVED] For-Loop Expression Limit
Stage05/Stage06 `parser.h` already parse full expressions (including comma operator)
for `for` init/cond/step slots via `parse_expr()`.

Issue #23 tracked the remaining Stage02 gap and is now fixed there as well.

### 47. [MISSING] stage07 / cc-x64 third-party C dialect gaps

**Status**: backlog, captured 2026-05-09. Surfaced by a probe-only
attempt to compile TCC (`https://repo.or.cz/tinycc.git`) with cc-x64
— see "TCC stress-test probe" notes below. Not pressing for selfhost,
but each gap blocks third-party C codebases of any size.

**Recently addressed**:

- **Anonymous struct/union members** (C11) — parser used to reject
  ```c
  union {
      struct { int a, b; };
      int c;
  };
  ```
  Heavy use in TCC (~40 sites; ~11 in `tcc.h` alone, including the
  load-bearing `SValue` type). Stage07 now keeps an unnamed aggregate
  member for layout/initialization and adds lookup aliases for nested
  member names. Covered by `selfhost/stage07/tests/test_phase27.c`.
- **Designated initializers** — global, static-local, and automatic-local
  aggregate initializers now support sparse array indexes, nested field/index
  chains such as `[2].op = 7` and `.nums[2] = 12`, string initialization
  of char arrays, and pointer/string relocations at designated offsets for
  static storage. Covered by `selfhost/stage07/tests/test_phase28.c` and
  `selfhost/stage07/tests/test_phase29.c`.
- **Block-scope compound literals** — scalar, struct, nested struct, and
  array compound literals now lower to hidden automatic locals with ordered
  initialization side effects. Address-taking and member access work through
  comma-expression lvalues. Covered by `selfhost/stage07/tests/test_phase30.c`.

**Surveyed but not yet hit (counts from TCC source)**:

| Feature | TCC use sites | Implementation notes |
|---|---|---|
| Flexible array members `int data[];` | 3 sites | Tiny sema special case |
| Bitfields | 1 site in `tcc.h` | Real codegen work — masks + shifts |
| Statement expressions `({ …; expr; })` | 1 site | GNU extension; nontrivial parser work |
| `_Atomic` / `typeof` / `__attribute__` / `inline` (with body) | Scattered | Most can be parsed-and-discarded |

**Header-set gaps** (would need to be added to
`selfhost/stage07/include/`): `stdarg.h`, `errno.h`, `setjmp.h`,
`fcntl.h`, `sys/time.h`, `dlfcn.h`. cc-x64 already handles `va_start`
/ `va_arg` / `va_end` as parser intrinsics, so `stdarg.h` is just
`typedef char *va_list;`.

**TCC stress-test probe (2026-05-09)**: cloned TCC into `/tmp/tcc-src`,
ran `./configure --cpu=x86_64 && make tccdefs_.h` to seed the predefs,
then `cc-x64 --hir -c tcc.c` against `selfhost/stage07/include` plus
shim headers in `/tmp/tcc-shim/`. First non-header failure was the
anonymous struct/union case above. Tree intentionally not committed —
this was an evaluation, not a port.

**Recommendation**: address only when forced by a real customer.
Compound literals are the next most likely C dialect blocker.

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

### 30. While Stage 4 slow32-dbt on ARM produces the correct artifacts, it does not execute the stage 06 compiler correctly itself. This is being investigated.
