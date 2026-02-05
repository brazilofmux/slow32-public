# SLOW-32 DBT Optimization Roadmap

This document tracks DBT-side opportunities, roughly priority-ordered. LLVM
already handles most low-level codegen work, so we only pursue items that
measurably reduce DBT overhead or correct DBT-specific behavior.

## Current State

At ~60% of native speed on integer workloads. Register cache on by default
(6-slot fully-associative). Stage 4 superblocks with deferred cold exits.
Peephole optimizer with JCC folding, dead store elimination, and NOP
compaction. Full `x64_insn_length()` coverage for all emitter-produced
instructions. Inline f32 FP via SSE. In-block back-edge loop optimization.

Code buffer: 41KB for bench-loops (105 blocks), 41KB for benchmark_core
(100 blocks). 324-471 peephole rewrites per workload.

### Key profiling data (benchmark_core, `-p` with sample rate 1)

| Metric | Stage 3 | Stage 4 |
|--------|---------|---------|
| Wall time | 0.156s | 0.046s |
| Translate | 0.000s | 0.001s |
| Exec (native code) | **0.156s (100%)** | **0.045s (98%)** |
| Dispatch (C loop) | 0.000s | 0.000s |
| Blocks | 125 | 100 |

**CRITICAL:** The default `-p` sample rate of 1/1000 produces garbage data
for workloads with <1000 dispatch iterations (~100-400 in practice). Use
sample rate 1 for accurate results on short workloads.

**All time is in native code execution.** Dispatch overhead is zero — block
chaining keeps execution in JIT code. The 3.4x Stage 3→4 speedup comes
from superblocks producing tighter native code (fewer register cache
flush/reload cycles, fewer block transitions, more straight-line code).

The bottleneck is **native code quality**, not dispatch or translation.

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
- Attribution: collect `-p` timing breakdown (sample rate 1) and `-s` stats.
- Hotspot proof: for micro-optimizations, show the affected opcode(s) or
  block(s) are among the hottest 5 (`-D -O`).

---

## Priority 1: Additional peephole patterns

**Why this is #1:** All execution time is in native code. Peephole
optimization directly improves the code that's running. The infrastructure
is solid (full instruction scanning, NOP compaction). Each pattern is
low-risk, additive, and immediately measurable.

**Patterns to implement:**
- `mov r, imm; op r, ...` → `op imm, ...` where x86 supports immediate
  operands (ADD, SUB, CMP, AND, OR, XOR, TEST with imm). Eliminates a MOV
  and frees a register.
- Redundant sign/zero extension elimination: `movzx eax, al` after an
  instruction that already zero-extended eax.
- Load-op fusion: `mov r, [mem]; op r2, r` → `op r2, [mem]` where the
  load result is only used once.

**Evidence gate:** Count pattern occurrences in `-O` output for the hottest
blocks. Measure code size and wall time before/after.

---

## Priority 2: Register cache efficiency

**Why:** The 3.4x superblock speedup proves that reducing prologue/epilogue
overhead matters enormously. Currently 6 registers are cached. The prescan
picks "top 6 by static use count" which may not match dynamic hotness.

**What to investigate:**
- Profile which cached registers are actually read/written in hot blocks
  (the `regcache=N/M` stat in `-D` output).
- Check if increasing cache size from 6 to 8 helps (more registers cached
  vs more prologue/epilogue overhead).
- Check if the prescan selection matches what the hot path actually uses.

**Potential improvements:**
- Increase register cache slots from 6 to 8 (or make configurable)
- Weight register selection by block execution count (profile-guided)
- Reduce prologue/epilogue cost by only saving/restoring dirty registers

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

## Priority 4: Code cache layout and I-cache pressure

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

### Cross-block register allocation for multi-block loops
**Evidence gate failed:** All hot loops in current workloads fit within
single blocks (handled by in-block back-edge optimization). Zero multi-block
back-edges observed across bench-loops, benchmark_core, and CSV validator.
Revisit if workloads with multi-block loops emerge.

### Superblock policy tuning
**Evidence gate failed:** Swept thresholds (1%, 3%, 5%, 10%, 20%), compared
two-pass vs single-pass, tested `-B` (avoid backedge extend). All produced
identical results — same superblock count, same code size, same performance.
Side-exit offender list is empty for all workloads. The branches being
extended past are all 0% taken, so any threshold permits extension. Nothing
to tune. Superblocks themselves are hugely valuable (3.4x on benchmark_core)
but the policy knobs are already at their optimum for current workloads.

### Dispatch overhead reduction (JALR)
**Evidence gate failed:** Accurate profiling (sample rate 1) shows dispatch
time is 0.000s — block chaining keeps all execution in native code. The
hottest JALR block (71-121 executions) resolves via inline lookup and chains
without returning to C. Only 28 inline lookup misses out of 373+ lookups.
The dispatch loop is not a bottleneck. Revisit if workloads with high
indirect-branch miss rates emerge.

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
