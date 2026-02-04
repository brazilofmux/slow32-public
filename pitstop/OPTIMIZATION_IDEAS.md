# SLOW-32 DBT Optimization Roadmap

This document tracks DBT-side opportunities, roughly priority-ordered. LLVM
already handles most low-level codegen work, so we only pursue items that
measurably reduce DBT overhead or correct DBT-specific behavior.

## Current State

At ~60% of native speed on integer workloads. 8 optimization phases already
shipped (see `tools/dbt/TODO.md`). Register cache is now on by default.
The remaining gap is a mix of:
- Dispatch/exit overhead on short blocks
- FP instructions going through a C helper call
- Superblock policy not yet tuned with real-world profiling data
- Intrinsic stubs paying full call overhead even for tiny sizes

## Guiding Principles

- Correctness is non-negotiable. Security is mandatory for a sandbox.
  Performance is maximized within those constraints.
- Prefer changes that reduce DBT overhead: exit frequency, dispatcher cost,
  cache misses, or translation-time waste.
- Every item below has an evidence gate. If we cannot measure it, it is not
  ready to implement.
- Measure before and after on at least two real workloads (benchmarks/ suite
  plus a real program like the CSV parser).

## Evidence Gates (Required Before Implementation)

- Stage comparison: measure `-1/-2/-3/-4` on at least two real workloads.
- Attribution: collect `-p` timing breakdown and `-s` block-cache stats.
- Hotspot proof: for micro-optimizations, show the affected opcode(s) or
  block(s) are among the hottest 5 (`-D -O`).

---

## Priority 1: Measure and Baseline

Before chasing any optimization, establish solid baselines.

### 1A. Comprehensive benchmark run

Collect `-p -s -D` output for all 5 benchmarks and the CSV workload across
stages 2/3/4. Record:
- Wall time, translate/exec/dispatch breakdown
- Cache hit rate, chain count, inline hit/miss ratio
- RAS hit rate
- Top 5 hottest blocks and their instruction mix
- Side-exit taken percentages

This data drives every decision below. Without it, priority ordering is
guesswork.

### 1B. Profile a real program (CSV parser or similar)

The benchmarks are tight integer loops. A real program exercises different
paths: function calls, string processing, memory allocation patterns, possibly
FP. Profile it the same way. The goal is to find whether dispatch overhead or
code quality dominates for real workloads.

---

## Priority 2: Inline FP operations (surgical subset)

**Current state:** Every FP instruction calls `dbt_fp_helper()`, which:
1. Flushes all pending writes
2. Flushes all dirty cached registers to memory
3. Sets up 5 arguments in SysV ABI registers
4. Calls through a function pointer (10-byte `mov r64, imm64` + `call`)
5. Reloads all cached registers from memory
6. The helper itself does a `switch` on opcode, then `memcpy`-based type
   punning, then the actual FP operation

That is ~40-50 host instructions of overhead per guest FP instruction, plus
the indirect call's branch prediction cost, plus the register cache
flush/reload destroying any cached state.

**Why this matters:** FP-heavy workloads (scientific computing, any future
graphics or DSP code) will be dominated by this overhead. Even a single
`FADD_S` in a hot loop means the register cache is worthless for that block.

**What to implement:**
- Inline the f32 arithmetic subset first: `FADD_S`, `FSUB_S`, `FMUL_S`,
  `FDIV_S`. These are just `movd xmm0, eax; movd xmm1, ecx; addss/subss/
  mulss/divss xmm0, xmm1; movd eax, xmm0` — about 4 instructions each.
- Inline `FEQ_S`, `FLT_S`, `FLE_S`: `comiss` + `setcc`.
- Inline `FNEG_S` and `FABS_S`: these are already bitwise ops on integers,
  so just emit `xor r32, 0x80000000` / `and r32, 0x7FFFFFFF` directly.
  (The helper already does this, but the call overhead remains.)
- `FCVT_W_S` / `FCVT_S_W`: `cvttss2si` / `cvtsi2ss`.

**Do not inline yet:** f64 operations (register pair complexity), `FSQRT_*`
(rare), conversions between f32/f64 (rare). Keep the helper for these.

**Evidence gate:** Profile a program with FP ops (write a small FP benchmark
if none exists — matrix multiply with floats, or a Mandelbrot set). Confirm
FP blocks appear in the top 5 hottest.

---

## Priority 3: Cross-block register allocation for self-loops

**Current state:** TODO item #6 in `tools/dbt/TODO.md`. Each block
independently allocates registers. When a hot loop's back-edge chains back
to the same block, the prologue reloads registers that were just flushed by
the epilogue.

**The in-block back-edge optimization (TODO #1) already handles this for
loops that fit within a single block.** The gap is loops that span 2-3
blocks (e.g., a loop body with a function call that gets inlined across
block boundaries).

**What to implement:**
- When a block's only predecessor is itself (self-loop via direct chain),
  emit a "steady-state" entry point after the prologue. The back-edge
  chains to the steady-state entry, skipping the load. The first entry
  uses the normal prologue.
- This is a special case of cross-block allocation but covers the most
  common hot pattern.

**Evidence gate:** Check `-D` output for blocks where `exec_count` is high
and the block chains to itself. Measure how much time the prologue/epilogue
consumes relative to the block body.

---

## Priority 4: Superblock policy tuning

**Current state:** Stage 4 extends past forward branches with
`taken_pct <= 3%` threshold. Two-pass profiling available via `-t`.
`avoid_backedge_extend` flag exists but is off by default.

**Known issues:**
- Stage 4 can regress when superblocks extend across hot back-edges.
- The 3% threshold is a guess. Different programs may want different values.
- Side-exit flush cost accumulates with superblock depth.

**Experiments to run:**
1. Sweep thresholds: `SWEEP_THRESHOLDS="1 3 5 10 20" ./run-benchmarks.sh`
2. Compare `-t` (two-pass) vs single-pass on each benchmark.
3. Try `-B` (avoid backedge extend) — does it help or hurt?
4. Measure side-exit taken rates with `-E -s` and correlate with regressions.

**What to implement (based on results):**
- If two-pass consistently helps, make it the default.
- If `-B` helps, make it the default.
- If optimal threshold varies per-program, consider adaptive thresholds
  (start aggressive, back off if side-exit rate exceeds N%).

---

## Priority 5: Dispatch overhead reduction

**Current state:** The native dispatcher trampoline (TODO #5) keeps
unchained transitions in generated code. But every block exit still:
1. Stores `exit_reason` to `[rbp + 0x84]`
2. Stores `pc` to `[rbp + 0x80]`
3. Jumps to shared exit stub or native dispatcher

For chained blocks, the overhead is just a `jmp rel32` (effectively free).
The cost is in unchained indirect branches (JALR) and first-time transitions.

**What to investigate:**
- What fraction of total exits are unchained? (`-s` shows chain_count vs
  total lookups.)
- For JALR-heavy code, does the inline lookup hit rate justify the code
  bloat? (4 probes × ~20 bytes each = 80 bytes per JALR site.)
- Is the hash function good enough? Collisions cause probe chains which
  cause inline misses which fall through to the native dispatcher.

**Potential improvements:**
- Increase inline probe count from 4 to 6 for programs with many indirect
  branches (configurable via flag).
- Consider a separate indirect-branch target cache (IBTC) with a different
  hash function optimized for return addresses.
- For the native dispatcher, consider a two-level lookup: small direct-mapped
  cache (256 entries, 1 probe) as first check, then fall through to the full
  linear-probe table on miss.

---

## Priority 6: Intrinsic stub size specialization

**Current state:** `memcpy`, `memset`, `memmove`, `strlen`, `memswap` are
intercepted and emitted as native stubs calling host libc. Bounds checks
added in commit 495249f.

**The problem:** For tiny copies (1-16 bytes), the call overhead (stack
alignment, function pointer load, call/ret) plus bounds checks (~12-16
instructions for memcpy) can exceed the cost of the copy itself.

**What to implement:**
- At translation time, if the size argument is a known constant (via
  `reg_constants[]`), emit inline loads/stores instead of a call.
  Example: 4-byte memcpy → `mov eax, [src]; mov [dst], eax`.
- For unknown sizes, keep the current stub but consider `rep movsb` for
  small-to-medium sizes (avoids function call overhead, ERMS makes this
  fast on modern CPUs).
- This only matters if intrinsic calls are frequent. Check with `-s`.

**Evidence gate:** Instrument intrinsic stubs to log call frequency and
size distribution. If most calls are >64 bytes, this optimization has
negligible value.

---

## Priority 7: Code cache layout and I-cache pressure

**Current state:** 4MB code buffer, linear allocation. Deferred side exits
are emitted at the end of each block (after the hot path). Superblocks can
be large (up to 4 extended branches × 64 instructions).

**The concern:** Cold side-exit stubs interleave with hot block code in the
same cache lines. If many blocks have side exits, the I-cache fills with
cold code.

**What to investigate:**
- Measure I-cache miss rate with `perf stat` on a long-running benchmark.
- Compare code buffer utilization (`-s` reports code_buffer_used).

**Potential improvements:**
- Split the code buffer into hot and cold regions. Hot path code goes at
  the front; deferred exits go at the back. This improves I-cache density
  for the hot path.
- This is a moderate refactor (cold stubs need to know the cold region
  base address, and chaining infrastructure needs to handle two regions).

---

## Lower Priority / Future Ideas

### Adaptive block size limit
`MAX_BLOCK_INSTS` is fixed at 64. Some hot functions might benefit from
larger blocks (fewer exits), while short blocks waste translation time.
Consider making this adaptive based on block execution count.

### Lazy constant materialization
Currently `LUI` + `ORI`/`ADDI` sequences each store to guest state. With
constant propagation, the intermediate `LUI` result could be folded into
the final operation. This already partially works but could be extended.

### Flag-based condition codes
SLOW-32 materializes comparison results into registers (`SLT`, `SEQ`, etc.)
then branches on them. The compare-branch fusion optimization already
handles the common case. Extending this to handle non-adjacent
compare-branch pairs (with intervening non-flag-clobbering instructions)
would eliminate more materializations.

### Profile-guided register allocation
Instead of prescan-based "top 6 by use count," use execution profiles to
weight registers by dynamic frequency. A register used once in a cold path
shouldn't displace one used in every loop iteration.

---

## Completed (from TODO.md)

1. ~~In-block back-edge loop optimization~~ (23.5% improvement)
2. ~~Direct host register ALU operations~~
3. ~~Dead temporary elimination~~ (Level 1 + Level 2 liveness)
4. ~~Peephole optimization enabled by default~~
5. ~~Native dispatcher trampoline~~
6. ~~SIB addressing for loads/stores~~
7. ~~Compile-time memory check specialization~~ (20-28 → 2-4 instructions)
8. ~~Intrinsic bounds hardening~~ (commit 495249f)
9. ~~MMIO overflow guards~~ (commit 495249f)
10. ~~Compare-branch fusion~~
11. ~~Constant propagation and folding~~
12. ~~Register cache enabled by default~~ (`-R` now toggles off)
13. ~~Peephole SIGILL fix~~ (x64_insn_length-based scanner advancement)
