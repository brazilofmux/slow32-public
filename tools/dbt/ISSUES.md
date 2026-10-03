# SLOW-32 DBT Emulator — Issues & Recommendations

This document tracks bugs, architectural limitations, and opportunities for improvement in the SLOW-32 Dynamic Binary Translator (`slow32-dbt`).

## Critical Bugs & Safety Issues

### 1. Out-of-Bounds Register Access for `f64` (Resolved)
The DBT's `load_f64_pair`/`store_f64_pair` and `load_u64_pair`/`store_u64_pair` helper functions access `regs[r]` and `regs[r+1]`.

- **Status**: Fixed in `64bb59b`. All register-pair access paths in `dbt_fp_helper` (including `FNEG.D`, `FABS.D`, and all 64-bit integer conversions) now use validated helper functions that check if the register index is even and < 31.

### 2. Unchecked Memory Allocations
Host-side allocations in `dbt_cpu_init`, `dbt_init_mmio`, and `translate_block` are often unchecked or only partially checked.

- **Problem**: `malloc`, `calloc`, and `mmap` failures can lead to null pointer dereferences or undefined behavior.
- **Recommendation**: Audit all allocation sites and ensure robust error handling (e.g., `exit(1)` or clean shutdown).

### 3. `dbt_write_callback` Bounds Check Overflow Risk
The check `if (size > cpu->mem_size || addr > cpu->mem_size - size)` protects against basic overflow.

- **Problem**: While correct, it's called for every section load. A large `size` close to `UINT32_MAX` could theoretically wrap in the subtraction if not careful (though `addr > ...` usually handles it).
- **Recommendation**: Use `addr + size < addr` or cast to `uint64_t` for robust overflow detection.

### 4. `yield_spin_count` False Positives
The `dbt_handle_yield` function detects a spin loop if `req_head` and `req_tail` don't change for 3 consecutive yields.

- **Problem**: Legitimate polling loops (e.g., waiting for console input or a timer) might yield frequently without new requests, triggering the "spin detected" warning incorrectly.
- **Recommendation**: Increase the threshold or only warn if `total_requests` hasn't changed over a longer period.

### 5. `lookup_table` Indexing Risk
The generated code masks the PC hash with `lookup_mask`.

- **Problem**: If `lookup_mask` is not a power of 2 minus 1, the masking logic fails. `block_cache_init` ensures power-of-2 size, but defensive programming would assert this relationship.
- **Recommendation**: Add a runtime assertion that `(size & (size - 1)) == 0`.

---

## Architectural Limitations & Performance

### 6. Single-Page `mmap` for Code Buffer
`dbt_cpu_init` allocates a fixed `STAGE1_CODE_BUFFER_SIZE` (64KB).

- **Problem**: For large programs or long-running sessions with many unique code paths, this buffer will fill up. There is no logic to flush the cache or grow the buffer.
- **Recommendation**: Implement a code buffer management strategy (e.g., ring buffer, flush-on-full, or `mremap` growth).

### 7. Global `mmio_state` Limits Concurrency
`mmio_state` is a static global variable in `dbt.c`.

- **Problem**: This prevents running multiple DBT instances in the same process (e.g., for testing or threaded simulation).
- **Recommendation**: Move `mmio_state` into `dbt_cpu_state_t`.

### 8. `rdtsc` Portability
The profiling code uses inline assembly `rdtsc`, which is x86-specific.

- **Problem**: The DBT won't compile on ARM64 (e.g., Apple Silicon) or other hosts.
- **Recommendation**: Use `clock_gettime` or compiler intrinsics for portable high-resolution timing.

---

## Quality of Life

### 9. Hardcoded Guest Memory Size
`GUEST_MEM_SIZE` is fixed at 256MB.

- **Problem**: This limits the simulation of smaller or larger systems and wastes host memory for small programs.
- **Recommendation**: Allow configuration via command-line flags, similar to the linker's `--mem-size`.

### 10. `math_intercepts` Table Maintenance
The table manually maps string names to host function pointers.

- **Problem**: Adding new intrinsics requires manual updates to this table, `slow32-tcg.c`, and potentially other places.
- **Recommendation**: Centralize intrinsic definitions in a shared header or generate them from a macro list.

---

## Latent Correctness Bug (from RV32IM sister project)

### 11. Self-Loop Back-Edge Register Mapping Corruption

**Severity**: Data corruption — silently produces wrong results.

**File**: `stage5_codegen.c`, function `cg_emit_self_loop_branch()` (line 204).

**Summary**: The self-loop optimization emits a back-edge that jumps directly to the loop body, bypassing the prologue register reload. It builds a "shuffle plan" to move registers to their expected entry slots. The bail-out guard at line 277 (`if (shuffle_count > 6) return false`) only checks shuffle **operations**, not total **distinct guest registers** used in the loop body. When a loop uses more guest registers than `STAGE5_RA_HOST_SLOTS` (8), the register allocator spills and repurposes slots during the block. The shuffle plan accounts for the final slot state but may not correctly handle all the intermediate slot reuse that occurred — particularly when a guest register was evicted mid-block, its slot reused for another guest register, and the shuffle plan must now reconstruct the original entry mapping from a state that has diverged in ways the slot-tracking can't fully represent.

**Trigger pattern**: GCC `-O2` struct copy loops. A 260-byte struct copy (e.g., dBASE's `value_t`: 4-byte type + 256-byte union) gets unrolled into a loop copying 24 bytes/iteration with 6 LW + 6 SW, using 9 distinct guest registers:

- 6 data temporaries (t3, t1, a7, a6, a0, a1)
- 2 pointer registers (source, dest)
- 1 end-of-range marker

With only 8 host register slots, the 9th register forces an eviction. The loop appears to work but silently truncates the copy.

**Observed symptom**: Large struct copies corrupted when compiled at `-O2`. The identical binary runs correctly in the interpreter. Compiling at `-Os` (which calls `memcpy` instead of inlining the loop) works around it.

**How this was found and fixed in the sister project**: The RV32IM DBT at `~/riscv/` had an equivalent bug in its simpler LRU register cache. The self-loop pre-scan collected source registers used in the loop body. When the count exceeded `RC_NUM_SLOTS` (8), LRU evictions during translation reshuffled the guest-to-host mapping. The back-edge jumped to `warm_entry` expecting the original mapping, but host registers now held wrong guest values.

**The RV32IM fix** (commit `a5ab60e` in `~/riscv/`): After the self-loop pre-scan, count distinct source registers. If `nused > RC_NUM_SLOTS`, disable the self-loop optimization entirely and fall back to normal flush-and-exit at the back-edge. The relevant code is in `~/riscv/dbt/dbt.c` around line 1092:

```c
if (self_loop) {
    int nused = 0;
    for (int r = 1; r < 32; r++)
        if (used[r]) nused++;
    if (past_first_branch || nused > RC_NUM_SLOTS)
        self_loop = 0;
}
```

**Recommended fix for SLOW-32**: The SLOW-32 codegen is SSA-based and more sophisticated than the RV32IM LRU cache, so the fix needs to be adapted. In `cg_emit_self_loop_branch()`, before or alongside the `shuffle_count > 6` check, count the total distinct guest registers that appear in `entry_gpr_for_slot[]` plus any guest registers that were allocated to slots during the block but are NOT in the entry mapping. If that total exceeds `STAGE5_RA_HOST_SLOTS`, return false (bail out to normal two-exit translation). The existing shuffle plan handles slot-to-slot moves and memory reloads, but it may not correctly reconstruct the entry state when heavy slot reuse has occurred.

**Reproduction**: Compile a C program with a 260-byte struct returned by value, with chained `identity(identity(make_str(buf)))` calls, at `-O2`. The test program `~/riscv/examples/test_struct_copy.c` demonstrates the pattern and can be adapted for SLOW-32's toolchain.

**Status (2026-07-09): INVESTIGATED — does not reproduce; directed coverage added.**
The referenced `stage5_codegen.c` self-loop shuffle plan no longer exists (dead
Stage 5 emitter removed in `dfb65921`). The equivalent machinery in the live
translators was audited and stress-tested:

- **AArch64** (`translate_a64.c`): immune by construction — register slots are
  assigned once by prescan and never reassigned mid-block, so the back-edge
  mapping cannot drift (`flush_cached_host_regs`, the only evictor, has no
  callers).
- **x86-64** (`translate.c`): the LRU cache *can* evict mid-block, but the
  back-edge emission compares against `backedge_snapshot` and, when unstable,
  does a full flush + reload from memory rather than a shuffle plan. Verified
  empirically: the 9-11-distinct-register unrolled copy loops in
  `regression/tests/bug-dbt-backedge-regpressure/` drive the UNSTABLE
  reconciliation path (confirmed via instrumentation) and produce correct
  results on both the stable and unstable paths, across the full flag matrix,
  on both host ISAs.

---

## 12. Prescan-Invisible Back-Edge → Pending-Write Corruption (FIXED 2026-07-09)

**Severity**: silent register corruption / crash. Found because
`cpp-exception-basic` memory-faulted in the DWARF CFI interpreter under the
default Stage 4 config on AArch64 (`scripts/diff-test.sh` caught it; Stage
1/2/3, `-R`, and `-S` all masked it).

**Bug chain** (AArch64): `execute_cfa_instructions` writes `r13 = r8 + 8` where
r13 is uncached, so the value rides in scratch W0 as a *pending write*. It is
lazily flushed (`str w0 → regs[13]`) inside the emitted code for the loop head
0xD954 — *after* `pc_map` recorded that PC's host offset. The loop's backward
branch lives beyond a `jal` that the prescan stops at, but superblock jump-over
inlining translates past the `jal` and reaches it; the in-block back-edge
optimization found 0xD954 in `pc_map` and emitted a direct branch to it. Every
loop iteration then re-executed the pending-write flush with whatever the loop
tail left in W0 (a ULEB byte), silently corrupting r13.

**Invariant**: a direct back-edge may only target a PC that was *known* to be a
back-edge target when it was translated (`is_backedge_target()`), because only
then were pending write/cond flushed and const-prop/bounds-elim reset *before*
the `pc_map` offset was recorded. A back-edge the prescan cannot see (its
branch lies beyond a JAL that jump-over inlining skips) violates this.

**Fix**: gate the in-block back-edge on `is_backedge_target(ctx, taken_pc)` in
both `translate_a64.c` and `translate.c`; on x86-64 additionally tag
`backedge_snapshot` with the guest PC it was captured at and require it to
match `taken_pc` (a block with two loop heads could otherwise reconcile
against the wrong snapshot). Unknown back-edges fall back to a normal chained
block exit — correct, and benchmark_core shows no measurable cost (its loop
heads are prescan-visible).

**Regression coverage**: test 5 of
`regression/tests/bug-dbt-backedge-regpressure/` reproduces the exact shape
(pre-fix: hangs/corrupts r24; post-fix: passes), alongside `cpp-exception-basic`
in the differential suite.

## 13. Select-Fusion In-Place setcc → Wrong Condition (FIXED 2026-08-24)

**Severity**: silent wrong answers on BOTH back ends, default config. Found by
`--paranoid-lite` on `lua/tests/control.lua`, where `repeat n = n * 2 until n
> 100` printed `2` instead of `128` (one iteration instead of seven). The a64
fusion shipped in `8621ab66`, the x64 one in `3d988e84`; the bug is in the
shared recognizer logic, so both carried it.

**Bug**: `select_idiom_scan()` recognizes LLVM's branchless select

```
C  = setcc a, b
M  = sub r0, C
t2 = xor T, F
u  = and t2, M
rd = xor F, u          ; = C ? T : F
```

and replaces the final XOR with `CMP a,b` + `CSEL`/`CMOV`. That re-materialized
compare reads `a` and `b` at the *fusion point*, so both must still hold their
original values there. The recognizer checked that with `defined_between()`,
which scans defs strictly **after** the setcc — and therefore cannot see the
setcc clobbering its own compare operand:

```
addi r3, r0, 255
sgtu r3, r1, r3        # in place: r3 is now the boolean, not 255
...
xor  r17, r1, r4       # fused CMP r1, r3 compares against 0/1
```

lparser.c's `luaK_...` register-limit check does exactly this, so the fused
condition became `r1 >u 1` — nearly always true — and the select returned the
wrong arm. The same blind spot was already known and handled one level down
(the in-place inner XOR is explicitly rejected); the setcc case was missed.

**Fix**: reject the fusion when the setcc's destination aliases either compare
operand (`if (c == a || c == b) continue;`) in both `translate_a64.c` and
`translate.c`. The leaf is often rematerializable from its def *before* the
setcc (x64 already has `select_operand_route()` for the analogous
after-the-setcc case) — a future win, not needed for correctness. Cost is one
fusion on the whole lua workload (2 → 1); benchmark_core is unchanged.

**Why the flag matrix missed it**: the fusion is not gated by stage or by
`-R`/`-S`, so `-1`, `-2`, `-3`, `-R`, and `-S` all reproduced it identically.
`S32_DBT_NO_SELECT_FUSE=1` is the knob that isolates it — add it to the triage
matrix.

**Regression coverage**: `regression/tests/bug-dbt-select-fuse-inplace-setcc/`
(in-place `sgtu` with the condition false, in-place `slt` on the first
operand, plus a non-in-place control). Pre-fix both back ends print `F1`;
post-fix both print `OK`.

## 14. x86-64 Stage-4 RTRIM$ Truncation (FIXED 2026-08-25)

**Severity**: silent wrong answer, x86-64 back end only, default config.

`sbasic/tests/stringfuncs` printed `[hello  ]` instead of `[hello]` for
`RTRIM$("hello   ")` — two trailing spaces survived. AArch64 was correct
(45/45), as was the reference interpreter; only `translate.c` at full Stage 4
was wrong. Only the default (reg cache + superblocks together) failed: `-1`,
`-2`, `-3`, `-R` and `-S` each produced the correct `[hello]`, and neither
`S32_DBT_NO_SELECT_FUSE=1` nor `SLOW32_DBT_NO_CHAIN=1` helped.

**Root cause**: `reg_alloc_prescan()` inlines forward `jal r0, target` while
scanning a block (to match what the translator does), advancing `pc` to the
jump target but the `decoded[]` index by only one. From that point on `pc` and
the index are no longer in lockstep. When the prescan later found an in-block
back-edge it located the loop head arithmetically:

```c
uint32_t target_idx = (target - start_pc) / 4;   // wrong after a JAL inline
```

which overshoots by exactly the number of instructions the JAL skipped. In the
failing block (guest `0x71CC`, `jal 0x71D0 -> 0x71E4`, 4 skipped) the loop head
`0x71E8` sits at index 3 but the formula yields 7, equal to `inst_count`, so
the "registers written in the loop body" scan covered only the back-edge branch
itself — which has no `rd`. `loop_written_regs` therefore came back **0**.

That matters because a deferred side exit captures its dirty-register snapshot
at translation time. The `bne r8, r4, 0x7208` exit at `0x71F0` is translated
*before* the `addi r3, r3, -1` at `0x71F4` exists, so r3 is clean in its
snapshot; correctness depends entirely on the back-edge handler promoting
`loop_written_regs` to dirty (translate.c, "Mark registers written in the loop
as dirty in all deferred exit snapshots"). With `loop_written_regs == 0` the
promotion never fired, and the emitted exit flushed only r8:

```asm
b6:  mov  DWORD PTR [rbp+0x20],r12d   ; r8 flushed
ba:  mov  DWORD PTR [rbp+0x80],0x7208 ; pc
c4:  jmp  <shared exit>               ; esi (r3) never written back
```

The guest then read the pre-loop `len` from memory. The observed r12/r3 deltas
were the *consequence* seen at `malloc`, not the cause: paranoid-lite's first
**soft** mismatch (`SLOW32_LITE_TRACE_SOFT=1`) was three blocks earlier at
`0x000071CC`, which is where the trim loop lives.

**Fix**: locate the back-edge target's index by pc lookup over `inst_pcs[]`
instead of the linear formula.

**Why a64 was unaffected**: `translate_a64.c`'s prescan ends the block at
`OP_JAL` rather than inlining it, so its indices stay in lockstep and the same
formula is correct there. A comment now records that invariant — if JAL
inlining is ever added to the a64 prescan, it must become a pc lookup too.

**Verification**: sbasic 45/45 on x86-64 (was 44/45) and 45/45 on a64;
regression and cross-engine differential green; `--paranoid-lite` clean over
stringfuncs on both back ends; `benchmark_core` checksum `0x8d70b2b` exact on
both; both builds warning-free.

**No directed regression test.** One was attempted and dropped rather than
shipped. A synthetic block reproducing the full pattern — profile-gated
superblock extension, an inlined forward JAL whose gap is wide enough to
overshoot past the loop's write, a genuine out-of-block deferred side exit, and
the written register arriving clean as a parameter — reaches an *identical*
`flushsnap` trace (`slot r3 dirty=0 -> skip`, `lwr=0x0`) and still computes the
right answer, so at least one further ingredient (probably which host register
r3 occupies, and whether the back-edge cache-stability reconciliation happens to
flush it) was not isolated. A test that passes both pre- and post-fix is worse
than none. Real coverage is `sbasic/tests/stringfuncs` run under the x86-64
DBT, which does discriminate.

### [RETRACTED, then reopened as a question] a64 vs x64 loop handling — and an unexplained 21%

**Original claim (2026-07-16, stood for about an hour):** `translate_a64.c` is missing
the loop pre-warm that `translate.c` has (16 sites of `loop_regs`; zero on a64), and this
likely explains the ~9.5 (x86-64) vs ~6-7.5 (Apple Silicon) spread.

**Retraction, same day:** the a64 translator doesn't need the pre-warm. The two files use
different register-cache designs:

- `translate.c` (x86-64): **lazy demand-driven LRU.** Registers load at first use, so a
  loop body contains loads — the pre-warm exists to hoist them out (evict non-loop regs
  at the loop head, load loop regs, record the pc_map offset after the loads so the
  back-edge skips them; plus `backedge_snapshot` stability machinery).
- `translate_a64.c` (AArch64): **static prescan allocation, never evicts mid-block**
  (comment at :1304). All eight slots load **once in the block prologue**
  (`reg_alloc_emit_prologue`); the back-edge is a **bare `b.cond`/`cbz`** to the loop
  head, no flush, no reload — stability *"guaranteed by AArch64's static prescan-based
  allocation."*

Both designs keep loop bodies free of cold loads. **Do not port the x64 pre-warm to
a64 — it has no target.** And do not read the cross-host BIPS spread as attributing
anything: Xeon vs Apple is different machines *and* different translators.

**What remains open, and is real:** on the same host (Apple M5 Max, 2026-07-16), on
identical kernels (`benchmark_core.c` at `BENCH_ITERS=100000000u`), `~/riscv`'s AArch64
DBT does 9.10 BIPS where this one does 7.50 — **21% faster per guest instruction, cause
unknown.** Candidate differences, none verified: adaptive LRU + warm-entry vs static
top-8 (matters only if a hot loop's working set isn't the block's top-8); instruction
selection quality; superblock policy; guest instruction mix (RISC-V's fused
compare-and-branch does more work per instruction, which makes per-instruction BIPS an
awkward metric across guest ISAs). **Next step is not borrowing features — it's
profiling: dump both emitted hot loops for the same kernel and count host instructions
per iteration.** Also note the a64 static design's known theoretical weakness (working
set beyond top-8 block-wide goes to memory on every access) has not been shown to fire
on any real workload.

**Update 2026-07-18 — the x86-64 run happened, and it shrinks this question.** dbase
reports on a Cascade Lake Xeon: slow32-dbt 212.0 s vs rv32-run 177.7 s = 1.19, against
the M5 Max's 1.38. Divide out the instruction-count component (~1.14, travels with the
binaries) and the per-guest-instruction translator ratio is **~1.04 on x64 vs ~1.21 on
a64**. So: the two x64 backends are near parity; the unexplained gap is specifically
between the two **a64** backends (this tree's static-prescan design vs riscv's
LRU+warm_entry — or Apple-microarch interaction with chained exits, or superblock
policy; still unattributed). Even a full a64 catch-up would leave dbase-on-Mac at ~48 s
vs riscv's 40 s, because the count component (compiler + fused branches) survives on
every host. Remaining open step, optional: disassemble both a64 hot loops for the same
kernel and count host instructions per iteration. Caveats: guests rebuilt on the Xeon;
count-ratio proxy is benchmark_core's, not dbase's.

**Update 2026-07-18, later — benchmark_core on the same Xeon flips the ordering.**
slow32-dbt 0.70 s vs rv32-run 0.81 s (median of 7; 4.07 vs 3.11 BIPS; both rebuilt at
100M, checksum 0x27dcb1c8). Same box, same day, dbase had rv ahead by 16%. So the
translator comparison is a function of (host, workload) — rv +21%/instruction on M5
kernels, s32 +31%/instruction on Xeon kernels, ~parity on Xeon dbase — and no constant
"faster translator" exists to go looking for. The a64 hot-loop diff remains the one
bounded, optional question. Provenance of the old "~9.5 BIPS (x86-64)": a Stage-5
profiling note ("0.03s, ~9.5 BIPS") — the 285M sprint on an unrecorded host — laundered
into EMULATORS.md by the 2026-07-02 doc-reconcile. Retired there with a full note.

## 15. x86-64 Branch-Against-Zero Compared Backwards (FIXED 2026-09-01)

**Severity**: silent wrong answer, x86-64 back end only, every config
(`-1`/`-C`/`-P` all reproduced). Invisible to the whole regression suite.

The self-hosted kit's `printf`/`sprintf`/`snprintf` produced literal text with
every conversion dropped — `"R=%d-%s"` with `(7, "ok")` printed `R=-`. The
reference interpreter and `slow32-fast` on the *same host* were correct, and so
was the AArch64 translator on the same binary, so this was `translate.c` alone.

**Root cause**: `translate_branch_common()` special-cased a zero operand to
`TEST` instead of `CMP`. `Bcc rs1, rs2` needs the flags of `rs1 - rs2`:

- `rs2 == 0` → `TEST h1,h1` is exact (`CMP rs1,0` also leaves CF=OF=0).
- `rs1 == 0` → the code emitted `TEST h2,h2`, which is the flags of
  `rs2 - 0` — the **reversed** comparison.

ZF is symmetric, so BEQ/BNE survived it; SF/OF/CF are not, so BLT/BGE/BLTU/BGEU
came out **inverted**. The failing block was two instructions:

```
0x2f00: addi r13, r1, 0
0x2f04: bge  zero, r11, 0x2ff8   ; r11 = 1, so 0 >= 1 is FALSE
```

`--paranoid-lite` pinned it precisely: every register matched, only PC diverged
(shadow `0x2f08`, DBT `0x2ff8`). Fix: materialize the zero and compare in the
correct direction (`XOR EAX,EAX; CMP EAX,h2`). Cache slots never alias RAX/RCX,
so RAX is safe scratch. `translate_a64.c` already did exactly this
(`emit_cmp_w32_w32(e, WZR, s)`, "flags must reflect (0 - rs2)"), which is why
AArch64 was unaffected. The `bne_compact`/`beq_compact` copies of the same
pattern are ZF-only and stay as they are.

**Why nothing caught it**: the differential harness runs only *clang*-built
binaries, and clang never emits `bge zero, rX` — it uses the other operand
order. The stage08 self-hosted compiler does emit it. The suite passed 82/82 on
this host both before and after the fix. **The coverage gap is the real
finding: kit-built binaries were never differentially tested.**

**Cost**: one extra `XOR` only on the `rs1 == 0` path. `benchmark_core`
unchanged at 0.07 s, checksum 0x8d70b2b.

## 16. AArch64 Code-Buffer Overflow Unguarded (FIXED 2026-09-08)

**Symptom**: the stage08-built SQLite shell (`sqlite/build-stage08.sh`,
1.9MB of guest code) died with SIGSEGV inside the translator itself --
`emit_patch_rel32` writing a branch patch at a page boundary -- while the
reference emulator, slow32-fast and the same guest under `--paranoid-lite`
all ran it to a clean halt with the right output.

**Cause**: the code buffer was 4MB and the AArch64 `translate_block_cached`
never checked the emitter's `overflow` flag.  Near the end of the buffer a
block's emits are silently dropped (emit32 sets the flag and returns), the
patch writes are unconditional and land past the mapping, and the truncated
block would otherwise have been committed and executed.  `translate.c`
(x86-64) has had both guards -- a headroom flush before translation and a
flush-and-retry on overflow -- since its own overflow work; the a64 port
never received them.  The shell translates to 6.5MB of host code, so it is
simply the first guest big enough to get there; every earlier workload fit.

**Fix**: the two x64 guards, ported verbatim (`cache_needs_flush`, the
`DBT_MAX_BLOCK_HOST_BYTES` headroom flush, and the overflow flush-and-retry
with a bail if a single block cannot fit); `emit_patch_rel32` refuses a site
past the capacity and sets the flag instead of writing.  And the buffer is
32MB (`CODE_BUFFER_SIZE`): at 4MB the shell flushed three times on a
seven-statement script, retranslating its working set each time; mmap
commits lazily, so small guests pay nothing.

**Verification**: shell output identical to the clang build's under the
DBT; `run-differential.sh` 88 agree with the four known qemu-only intrinsic
divergences; `run-kit-differential.sh` all engines agree.  Same lesson as
DBT-15: the clang-built suite never produced a guest this large.

## 17. AArch64 In-Place Branch Patches Were Unbounded (FIXED 2026-09-08)

DBT-16 guarded `emit_patch_rel32` and taught `translate_block_cached` to
flush for headroom and to discard an overflowed block.  It missed that
`translate_a64.c` patches branches in eleven *other* places by writing
straight through `e->buf + off`, and it left the intrinsic path outside
both guards.

**Symptom**: the stage08-built SQLite shell, run with a deliberately small
code buffer, SIGSEGVs in `emit_mem_access_check` -- a translator crash, not
a bad guest.

**Cause**: `emit_inst` stops *writing* once a block overflows but does not
advance `offset`, so `emit_offset()` saturates at `capacity`. A patch
offset recorded there is itself in range while the four-byte write at it is
not, and it lands on the page after the mapping. Separately,
`try_emit_intrinsic_a64` ran before the flush checks, sized its emit
capacity from the unaligned `code_buffer_used` (up to 15 bytes too
generous), and committed without ever testing `overflow`.

**Fix**: every in-place patch goes through `a64_patch_ptr`, which returns a
scratch word and sets `overflow` rather than writing out of bounds; both
flushes run before the intrinsic attempt (the x64 order); the intrinsic
path sizes capacity from the aligned start and declines on overflow, so the
caller falls through to a normal translation; block and code-pointer
allocation retry once after a flush; and `cache_record_exit` drops an
out-of-range exit instead of recording one that chaining would later write
through, warning once rather than per site.

**Verification**: with `CODE_BUFFER_SIZE` cut to 256KB and
`DBT_MAX_BLOCK_HOST_BYTES` to 1KB -- a mutation that makes the buffer tail
genuinely reachable -- the shell went from SIGSEGV to correct output.  At
production sizes it is a no-op: `sqlite3.c` through the stage08 compiler is
44s both before and after, with identical block, chain and flush counts and
byte-identical assembly output; `benchmark_core` 0.03s, checksum 0x8d70b2b.

**Process note**: two intermediate readings during this work looked like a
17x regression and were reported as one. Both were the machine thrashing
under orphaned jobs left by cancelled verification runs, not the code. Kill
the children, not just the shell, and re-measure on an idle box before
believing a performance delta.

### 2026-09-13 review with fresh eyes: where the a64 translator's time goes, kernel by kernel

Measured, not read (Apple M-series, `examples/benchmark_core.c` split into one-kernel
binaries at BENCH_ITERS=300M via the `*_iters = 1u` trick; best of 3-5; `~/riscv`'s
`rv32-run` built from the same source with the header's gcc line). Guest instruction
counts from `slow32-fast`: arith 2.40G, branch 5.25G, mem 0.90G.

| kernel | slow32-dbt | rv32-run | slow32-dbt -U | -S | -U -S | -2 / -3 |
|---|---|---|---|---|---|---|
| arith  | 0.36 s | 0.36 s | 0.36 | | | |
| branch | 0.64 s | 0.47 s | 0.55 | 0.59 | 0.58 | 0.63 / 0.63 |
| mem    | 0.05 s | 0.04 s | 0.04 | | | |

So the "unexplained 21%" above is one kernel: **branch**. Arith is at parity and
dependency-chain bound (8 host insns for 8 guest, `-d` shows the loop as
`add; lsl; eor(shifted); lsr; eor(shifted); add; subs; b.ne` -- the two shifts are
dead temporaries kept alive because the block cannot see the redefinition; 1 of 8).
Mem runs 0.9G guest insns in 53 ms, correct checksum, ~3 cycles an iteration for a
load, a store and two bounds checks: the core swallows it, and `-U` buys nothing there.

**Branch, read from `-d 100` host dumps (objdump in the toolchain container):** the
loop body inside a block is 21 host insns for 17 guest. What costs is leaving the
block, which the kernel does every other iteration (its `r5 += 3 & 7` parity
alternates paths). A transition is: flush 5 cached regs, store the PC, then either a
direct `B` (if the target was already translated when this exit was emitted) or the
compact-table probe -- `mov; add; ldr; mov; cmp; b.ne; ldr x4; br x4` -- and the
target's prologue reloads all its cached regs (7 `ldr`). Two transitions per even
iteration, ~60 host insns against 21 on odd ones.

**The one structural finding:** `emit_exit_chained` (a64) emits a direct `B` only
when the target already exists; otherwise it emits the probe and leaves the *fallback*
`b` after the probe as the patch site. When the target is translated later, the probe
is never upgraded: `-S -d 100` on the branch kernel shows both edges of the
`andi; bne` block still going through the probe and an indirect `br` at the end of a
300M-iteration run. Every edge whose target came second pays ~9 instructions and an
indirect branch per traversal for the life of the process. Upgrading the patch to
overwrite the probe's first instruction with the direct `B` (the pending-patch
machinery and `cache_record_exit` already exist) is the obvious thing; whether the
prologue reload can be skipped on a chained entry with a compatible register set is
the larger design question the riscv sibling's warm-entry answers and this static
prescan design does not.

**Smaller, from the same dumps:** `sub rd, zero, rs` is `mov w0,#0; sub` (2) where
`neg` is 1; `seq/sne` + negate is `cset; mov; sub` (3) where `csetm` is 1 -- both in
the branch loop; a bounds check materialises each limit with `mov+movk` (4 of 9
insns) where a pinned register or a single unsigned range compare would do -- but
measured cost on these kernels is nil, so it is a code-size point only.

**The probe upgrade, done and measured (same day).** `emit_exit_chained` now records
the probe's first instruction as the pending patch site, so a later-translated
target turns the probe into a direct `B` (`-S -d 100` shows it). The measurement
was the lesson: a plain A/B on the full benchmark said the change was 17% SLOWER,
reproducibly, across five guest-side paddings -- and the guest paddings never
moved the host code. `DBT_LAYOUT_PAD` (new, dbt.c) shifts the translated blocks
by N*16 bytes; over eight pads the unpatched build is 0.28 s at pads 0 and 64
and 0.31 everywhere else, the patched one 0.30-0.33 everywhere: **a 64-byte
placement effect of ~10% on the hot loop, and the change itself neutral within
noise.** The `-U`/`-S` "speedups" recorded above were the same placement luck.
Rule, as for cc-x64: time emitted-code changes across pads and compare medians;
one placement is not a measurement. The probe upgrade stays because it is
strictly less code on the path (no probe, no indirect branch) and correct
(checksums, gates), not because the benchmark rewarded it -- the remaining
transition cost is the flush and the prologue reload, which this does not touch.
`DBT_CHAIN_TRACE=1` prints every resolved pending chain (source block, exit,
target, host entry, compact-table entry) and was what showed the hot edges.

**Tooling gaps hit on the way:** `-d`'s "hottest blocks" ranking counts dispatcher
entries, so a chained loop shows 0 executions and a `ret` stub shows as hottest;
`-Q` only samples whole seconds; `-X` prints nothing unless `-d` or `-O` is also
given; the host disassembly shells out to `objdump` and fails on macOS (the raw
`/tmp/slow32-dbt-host-*.bin` files are still written -- feed them to
`objdump -D -b binary -m aarch64` in the `slow32:toolchain` container).

## 18. Self-Hosted dbt-a64 Segfaults on COBOL Programs (FIXED 2026-09-30)

`selfhost/stage08-cross-a64/out/dbt-a64` exited 139 on any COBOL program.
The line that mattered: a C program linked with libc_debug ran, the same
program linked with libc_mmio crashed.  COBOL was only the first MMIO
user anyone ran.

Cause: both cross trees' libcs (`libc_a64/mmio_ring_a64.c`,
`libc_x64/mmio_ring_x64.c`) kept a hand-written mirror of
`mmio_ring_state_t`, "byte-for-byte compatible since dbt allocates
these".  The real struct had since gained the DPC indices, the timer and
posted-read arrays, the guest-memory hooks and `dpc_ring` -- ahead of
`base_addr`, `req_ring` and `data_buffer`.  dbt.c, compiled against the
real header, and the stub, against its copy, disagreed on every later
field; the stub set the ring pointers at the wrong offsets and the first
MMIO request dereferenced garbage.  Fix: the stubs include
`tools/emulator/mmio_ring.h` itself.  No copy to drift.

With the crash gone, what the crash had hidden:
- **Floating point and the intrinsic functions gave 0.**  dbt.c
  intercepts guest libm calls with host functions, and the self-hosted
  build links placeholders (`libc_a64/libc_extra.c`: exp, log, pow, sin
  ... return 0; floor returns x).  Even the real ones there, sqrt and
  fabs, came back wrong: the trampoline passes the double in d0, and a
  function cc-a64 compiled does not read it there.  Under `__S12CC__` (the
  self-hosted compilers' macro) the table is empty and the guest's own
  libm runs.
- **The clock read 1969-1970**, and SQLite's reads failed: the stubs
  answered GETTIME, READ_DIRECT, ACCESS, UNLINK, FTRUNCATE, GETCWD and
  LSTAT with ERR.  Each is now one Linux syscall, as the real ring does.
  `S32_MMIO_TRACE=1` prints each request the stub answers with an error,
  `=2` every request (op, status, offset, length, then the response).
  That is how the gaps showed themselves.

Verified: 60 of the harness's `free/` programs under dbt-a64 in
linux/arm64 (gcc:latest, podman) against the native DBT: 58 identical,
the two ESQL programs not (DBT-19).  C programs using libm, time() and
localtime agree.  Both cross trees pass `make && make test` as the
builder runs them (a64 in the container, x64 on kagura).  dbt-x64 runs
the MMIO programs too (DBT-20 for what it does next).

## 19. SQLite Fails Under the Self-Hosted dbt-a64 (FIXED 2026-09-30)

`cobol/tests/free/esqldesc` and `esqldyn` (EXEC SQL on SQLite): the
first statement fails with "malformed database schema (sqlite_master) -
invalid rootpage", and `PUBLIC.db` stays empty.  The gcc-built DBT in
the same container, and `s32fast-hir` (the interpreter cc-a64 compiles,
with its own MMIO), both run it correctly.  Ruled out, each by a
differential:
- the MMIO responses: the op/status/length sequence matches the real
  ring up to the point where the guest itself takes another path;
- the stat payload, byte for byte;
- floating point, every conversion and comparison;
- the intrinsics and hooks (-I);
- the translation stages (-1 .. -4);
- `--paranoid-skip 17000`: no divergence between translation and
  interpretation (the `fstat` block's divergence is an artifact of
  lockstep over MMIO; the native DBT shows it too);
- the shared instruction decoder: `decode_instruction` compiled by
  cc-a64 and by gcc decodes all 302,344 words of the program alike.
The guest diverges right after the `fstat` of the database file: the
real run goes on to ACCESS, SEEK and a 16-byte header READ; this one
stops.  Since translation and interpretation agree, the wrong value
comes from something both share that is compiled by cc-a64 and was not
covered by the differentials above -- or from guest memory written host
side.  Next: a guest-state checkpoint differential (registers and a
memory hash at each block exit) between the gcc DBT and dbt-a64, from
the `fstat` return on.

Fixed with that differential, done in Stage 1 (`-1`, one block per
dispatcher trip, and it still failed there): after every block, the
block PC, the next PC and a hash of the 32 registers, from the gcc DBT
and from dbt-a64, then the first line that differs. The run is under
200,000 blocks, so every block was traced.

- The first difference was not the bug: an MMIO error response. The
  real service carries the errno in `length` (mmio_fail); both cross
  trees' stubs left it 0, so every failed request reached the guest as
  EIO, here a GETENV of an unset variable (EINVAL). Fixed: the stubs
  record the kernel's errno, EBADF for an unmapped fd, EINVAL otherwise.
- With registers the same, the first control-flow split was in
  sqlite3InitCallback: `sqlite3GetUInt32(rootpage)` returned 0, which
  is "invalid rootpage". Its tail block, `seq r1, r1, r5; bne r1, zero`,
  branched correctly and left r1 (the return value) 0. The fused
  compare-branch skips materializing rd when `dead_temp_skip` says it is
  dead, and dbt-a64 read the flag at index 0 (the `andi`, truly dead)
  instead of 2. `pending_cond.inst_idx` held 2 when stored and when the
  branch began; the local copy lost it. cc-a64 had paired the loads of
  `imm` (+8) and `inst_idx` (+12) into one LDP while `inst_idx`'s value
  was spilled: ra_reg -1 encoded as register 31, `ldp w24, wzr` and
  `str wzr` to the spill slot. `hx_load_pair_safe` never checked that
  both loads had registers (the store twin did). selfhost/stage08-cross-a64
  hir_codegen_a64.h now refuses; cc-x64 has no LDP pairing.

esqldesc and esqldyn match their expected output under dbt-a64 and
dbt-x64. Found on the way, not fixed: the self-hosted libcs' printf
prints `%-2d` literally and shifts every later argument; cc-a64's
`offsetof` refuses a nested member designator (`offsetof(T, a.b)`).

## 20. Floating Point Broken in the Self-Hosted dbt-x64 (FIXED 2026-09-30)

With DBT-18 fixed, dbt-x64 (built by cc-x64, run on kagura, x86-64
Linux) runs MMIO programs, and shows its floating point is wrong: a C
`sqrt(2.0)` prints -2147483648, COBOL `FUNCTION SQRT(2)` prints
+4609047870845170000, and two programs then segfault.  The a64 DBT's
equivalents are right.  The x64 translator's FP path and
`libc_x64/fpu_ops.c` (cc-x64-compiled) are the first suspects; the
equivalent a64 bug was in the math intercepts, which are already off
here.  Nothing in the fleet runs dbt-x64 on FP code; `make test` does not.

Cause: not the translator, the compiler that built it. The DBT's C
helpers, which run the FP instructions the translator does not inline,
were miscompiled by cc-x64: `sqrt` came back as its argument's bits
converted as an integer, `(float)(int64_t)v` moved the bits across
unconverted, and the unsigned conversions (FCVT.D.WU, .S.LU, .D.LU)
took the signed ones. The segfaults were a third defect: cc-x64 pointed
every function pointer in a static initializer at the start of .data,
so hooks.c's table called into data the first time a hook ran
(`printf` of a 64-bit value, `__udivdi3`). `-H` ran on; `S32_HOOKS`
put it on the unsigned divisions.

Most of it was the shared stage08 front end, wrong on SLOW-32 too, and
fixed there with the other defects the search turned up: selfhost
ISSUES-72. fp.s32x (the FP instructions and conversions, printed as
bits) now matches the native run under dbt-x64 with hooks on, and
comp12, floatmf, intrinsics, fnreturn, fnargbad, fnvalues and intr2002
from cobol/tests match slow32-fast.

## 21. memchr Is a Native Routine (2026-10-02)

Not a defect: an addition, recorded here because it is a stub on both
hosts and the next reader of a fault in one will want to know what it
promises.

A line sequential READ in the COBOL runtime finds each record's end with
`memchr` -- 379 guest instructions a record in the reports of majesty's
batch, a sixth of what the largest of them executes.  `memcpy`,
`memset`, `memmove`, `strlen`, `memswap` and `memcmp` were native;
`memchr` was not.  It is now (`emit_native_memchr_stub` in translate.c,
`emit_native_memchr_stub_a64` in translate_a64.c), found by name as the
others are, and like them total: it cannot hand a case back.

What it promises is the guest loop's behaviour, which reads as far as
the first match and no further:

- the search is over what memory there is from `s` -- a count that
  reaches past the end of memory is not a fault when the byte is found
  before it (`memchr(p, 0, SIZE_MAX)` is how a C library spells
  rawmemchr);
- not found, and the count wanted more than memory had: the fault the
  guest's loop would take, a load at the first address past memory;
- `s` itself past memory with a count: that fault, at `s`;
- a count of zero reads nothing, whatever `s` is.

Tests: `regression/tests/stdlib-memchr` prints what every kind of call
returns (so the differential compares each engine's answer with the
reference's loop), and `bug-dbt-intrinsic-bounds-memchr` and
`-memchr-start` take the two faults.  Nine mutants of the arm64 stub,
all caught.  The x86-64 stub was run as dbt-x64 (cc-x64's build) in an
amd64 container under emulation on the arm64 Mac -- the three tests
right, and wrong when the stub was broken on purpose -- but not on
x86-64 hardware; the next builder run is that.  qemu has no such stub
and walks the bytes, but says nothing at a fault, so the two fault
tests join the known qemu-only divergences in `run-differential.sh`.


## 22. The Block-Length Exit Was Never Chained (FIXED 2026-10-03)

A translated block ends at a branch, a jump, a call -- or at
`MAX_BLOCK_INSTS` (96), and that last exit was `emit_exit(EXIT_BLOCK_END)`:
a return to the C dispatcher, which looks the next PC up and jumps to its
block.  A straight run of code longer than 96 instructions therefore paid
a dispatcher trip every time it executed, for the life of the process,
while its branch exits were chained once and never again.

Found by the COBOL islands: kmove's loop, 0.10 s under the DBT, became
0.21 s when the compiler turned its divisions by ten into the reciprocal
(mulhu, srli, mul, sub -- four instructions for two), though it ran only
4% more instructions.  The DBT's statistics said why before the
disassembly did: 3,000,000 cache lookups against 626, one per iteration,
and a new knob (`SLOW32_DBT_DISPATCH_HIST=1`, printed with the statistics)
named the PC -- the middle of straight-line code, 0x660, where nothing
jumped.  The run had passed 96 instructions.

Both translators' length exits (`translate_a64.c`, the stage-4 path and
the cached-return one) are `emit_exit_chained` now, with the exit's
branch_pc recorded: the same exit a plain jump makes, patched into a
direct B when the next block exists.  kmove 0.21 -> 0.10 s; kedit's
inline editing, 0.35 -> 0.27 s, had been paying the same.  The
differential harnesses (run-differential, run-kit-differential,
run-kit-tools-differential) all agree; the six qemu-only divergences are
the known ones.  Also: MULH and MULHU go through the register cache like
MUL and DIV (they loaded and stored the guest register file), and the
histogram knob stays.

The x86-64 translator (translate.c) had the same exit and takes the same
fix, which no machine here can run: the builder's amd64 differential is
its test (as with DBT-16's x64 back-edge fix).  Its mulh/mulhu already
went through the register cache.

Lesson, again: a `git stash` / `make` / `git stash pop` / `make` leaves
the popped source with the same second's mtime as the object built from
the stashed one, and `make` keeps the wrong binary.  Three differential
runs went against HEAD's DBT before `touch` showed it.  Don't stash in
this tree; build with the tree as it is.
