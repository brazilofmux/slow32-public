# SLOW-32 DBT Optimization Roadmap

This document tracks DBT-side opportunities, roughly priority-ordered. LLVM
already handles most low-level codegen work, so we only pursue items that
measurably reduce DBT overhead or correct DBT-specific behavior.

## Current State

At ~60% of native speed on integer workloads. Register cache on by default
(6-slot fully-associative). Stage 4 superblocks with deferred cold exits.
Peephole optimizer with JCC folding, dead store elimination, and NOP
compaction. Inline f32 FP via SSE. In-block back-edge loop optimization.

Code buffer: 42KB for bench-loops (105 blocks), 55KB for CSV validator
(145 blocks). 265-395 peephole rewrites per workload.

Remaining gap is a mix of:
- `x64_insn_length()` can't decode all instructions → blocks skip compaction
- Intrinsic stubs paying full call overhead even for tiny sizes
- Superblock policy not yet tuned with real-world profiling data
- Cross-block register allocation for multi-block loops

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

## Priority 1: Cross-block register allocation for self-loops

**Current state:** Each block independently allocates registers. When a hot
loop's back-edge chains back to the same block, the prologue reloads
registers that were just flushed by the epilogue.

The in-block back-edge optimization already handles loops that fit within
a single block (23.5% improvement). The gap is loops that span 2-3 blocks.

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

## Priority 2: Superblock policy tuning

**Current state:** Stage 4 extends past forward branches with
`taken_pct <= 3%` threshold. Two-pass profiling available via `-t`.
`avoid_backedge_extend` flag exists but is off by default.

**Known issues:**
- Stage 4 can regress when superblocks extend across hot back-edges.
- The 3% threshold is a guess. Different programs may want different values.
- Side-exit flush cost accumulates with superblock depth.

**Experiments to run:**
1. Sweep thresholds: 1%, 3%, 5%, 10%, 20%.
2. Compare `-t` (two-pass) vs single-pass on each benchmark.
3. Try `-B` (avoid backedge extend) — does it help or hurt?
4. Measure side-exit taken rates with `-E -s` and correlate with regressions.

**What to implement (based on results):**
- If two-pass consistently helps, make it the default.
- If `-B` helps, make it the default.
- If optimal threshold varies per-program, consider adaptive thresholds.

---

## Priority 3: Intrinsic stub size specialization

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
- For unknown sizes, keep the current stub but consider `rep movsb`.
- This only matters if intrinsic calls are frequent.

**Evidence gate:** Instrument intrinsic stubs to log call frequency and
size distribution. If most calls are >64 bytes, this has negligible value.

---

## Priority 4: Dispatch overhead reduction

**Current state:** Chained blocks are just `jmp rel32` (effectively free).
The cost is in unchained indirect branches (JALR) and first-time transitions.
Inline lookup uses 4 probes per JALR site.

**What to investigate:**
- What fraction of total exits are unchained? (`-s` shows chain_count vs
  total lookups.)
- For JALR-heavy code, does the inline lookup hit rate justify the code
  bloat? (4 probes × ~20 bytes each = 80 bytes per JALR site.)

**Potential improvements:**
- Increase inline probe count from 4 to 6 for indirect-branch-heavy code.
- Consider a separate IBTC with a different hash function optimized for
  return addresses.
- Two-level dispatch: small direct-mapped cache (256 entries) as first
  check, then fall through to full table.

---

## Priority 5: Code cache layout and I-cache pressure

**Current state:** 4MB code buffer, linear allocation. Deferred side exits
emitted at end of each block (after the hot path). Superblocks can be large.

**What to investigate:**
- Measure I-cache miss rate with `perf stat` on a long-running benchmark.
- Compare code buffer utilization.

**Potential improvements:**
- Split code buffer into hot and cold regions.
- This is a moderate refactor.

---

## Lower Priority / Future Ideas

### Adaptive block size limit
`MAX_BLOCK_INSTS` is fixed at 64. Some hot functions might benefit from
larger blocks. Consider making this adaptive based on block execution count.

### Lazy constant materialization
`LUI` + `ORI`/`ADDI` sequences each store to guest state. With constant
propagation, the intermediate `LUI` result could be folded into the final
operation.

### Flag-based condition codes
Extending compare-branch fusion to handle non-adjacent compare-branch pairs
(with intervening non-flag-clobbering instructions) would eliminate more
materializations.

### Profile-guided register allocation
Instead of prescan-based "top 6 by use count," use execution profiles to
weight registers by dynamic frequency.

### Additional peephole patterns
- `mov r, imm; op r, ...` → `op imm, ...` where supported
- Redundant sign/zero extension elimination
- Load-op fusion for memory operands

---

## Completed

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
14. ~~Inline f32 FP operations~~ (14 opcodes via SSE)
15. ~~Peephole JCC folding~~ (Jcc +5; JMP → inverted Jcc)
16. ~~Peephole dead store elimination~~ (duplicate mov [rbp+disp], imm32)
17. ~~NOP compaction~~ (removes peephole NOPs, adjusts branches)
18. ~~x64_insn_length REX.W bugfix~~ (rex & 0x48 → rex & 0x08)
19. ~~Expand x64_insn_length() coverage~~ (added 0x88, 0xBE/0xBF, 0xCC, C7 memory forms; +19-22% more peephole rewrites, ~1KB code savings)
