# SLOW-32 DBT Optimization Roadmap

This document tracks DBT-side opportunities, roughly priority-ordered. LLVM
already handles most low-level codegen work, so we only pursue items that
measurably reduce DBT overhead or correct DBT-specific behavior.

## Current State

At ~60% of native speed on integer workloads. Register cache on by default
(8-slot fully-associative). Stage 4 superblocks with deferred cold exits.
Peephole optimizer with JCC folding, dead store elimination, and NOP
compaction. Full `x64_insn_length()` coverage for all emitter-produced
instructions. Inline f32 FP via SSE. In-block back-edge loop optimization.

Code buffer: ~40KB for benchmark_core (100 blocks), ~96KB for CSV
validator (211 blocks). 356 peephole rewrites on benchmark_core, 939 on
CSV validator. Branch comparisons now use cached registers directly.

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

## Priority 1: Remaining code quality opportunities

**Status:** Peephole and emitter-level improvements are largely done for
branch/comparison code. The remaining waste falls into distinct categories.

### 1a. MOV-ALU folding (peephole)

`mov rA, rB; alu rX, rA` → `alu rX, rB` when rA is dead. Generalizes
MOV-CMP to XOR, AND, OR, ADD, SUB. Observed in benchmark_core block
0xC70 (e.g., `mov eax,esi; mov ecx,ebx; xor ecx,eax`). However, these
only appear in low-execution blocks (5 execs). **Low payoff.**

### 1b. ~~Emitter-level ALU with cached registers~~ (DONE)

**Completed.** See item #25 in the completed list. ALU reg-reg ops
already had cached register support in their `hd != X64_NOREG` branches.
The remaining waste was SETcc materialization always going through RAX
(fixed: SETcc+MOVZX directly into cached hd) and SUB rd==rs2 using 3
instructions (fixed: NEG+ADD when both operands cached). Remaining MOVs
in these blocks are inherent 2-operand x86 copies (rd != rs1).

### 1c. ~~Redundant bounds check elimination~~ (DONE)

**Completed.** See item #22 in the completed list.

The biggest source of code bloat is memory access bounds checking. Each
LDW/STW emits ~50-60 bytes of host code on the hot path (LEA, CMP, Jcc,
fault exit stub, then the actual load/store). In the CSV hot loop (block
0xC7DC, 10 guest insns, 378 host bytes), 4 memory accesses account for
~230 of the 378 bytes.

**Concrete opportunity in block 0xC7DC (285 execs):**

```
ldw r1, r12+44    -- bounds check #1: validate r12+44
add r1, r1, r20
stw r12+44, r1    -- bounds check #2: same addr (also W^X check)
add/sub/add       -- r12 unchanged
beq r14, r15
ldw r3, r12+48   -- bounds check #3: r12+48, only +4 from validated
ldw r1, r12+44   -- bounds check #4: SAME addr as #1
bne r3, r1
```

Since r12 is never modified in this block, checks #3 and #4 are provably
redundant (r12+44 validated ⇒ r12+48 also in bounds since both are
4-byte aligned and mem_size is page-aligned). Check #2 needs W^X but not
range re-validation.

**Potential savings:** 2-3 bounds checks eliminated = 100-150 bytes of
host code removed from the hot path of a 285-exec block.

**Implementation approach:** Track, per block, which `(base_reg, offset)`
pairs have been validated. Before emitting a bounds check, look up
whether the same base_reg has an existing validation that covers the new
offset (prior_offset ≤ new_offset ≤ prior_offset + validated_size).
Invalidate on writes to base_reg. Conservative: only track within a
single block (resets at block boundaries and branches).

**Complexity:** Moderate. The bounds check emission is centralized in
the load/store translation, so the change is localized. The tracking
state is small (a few validated ranges per block). The tricky part is
ensuring the invariant holds across all paths through the block — if
there's a branch that could skip the initial check, the later access
can't rely on it. Superblocks make this harder since they have
side exits between instructions.

### 1d. Remaining peephole patterns (lower value)

- `mov r, imm; op r, ...` → `op imm, ...` (immediate folding)
- Redundant sign/zero extension: `movzx eax, al` after SETcc
- Load-op fusion: `mov r, [mem]; op r2, r` → `op r2, [mem]`

These occur but are not concentrated in hot blocks.

### Hot block inventory (both workloads)

**benchmark_core:**
- 0xD0A8 (121 execs, 417B): JALR inline lookup. Structural duplication
  (4-probe hash × 2 paths). Not peephole-addressable.
- 0xC70 (5 execs, 384B): Arithmetic superblock. MOV-ALU redundancy but
  low exec count.

**validatecsv (csv2/descriptions.csv):**
- 0xC7DC (285 execs, 322B): CSV inner loop. 10 guest insns. Dominated by
  bounds check overhead (3 memory ops). Branch comparisons use cached
  regs directly. Loads/stores use cached host registers directly (no
  scratch RCX copy).
- 0x210 (285 execs, 150B): Setup/call block. Lean.
- 0x1678 (179 execs, 390B): Same structure as benchmark_core 0xC70.
  SETcc now goes directly into cached host registers. SUB rd==rs2
  uses NEG+ADD when both operands are cached.
- 0xEAAC (68 execs, 417B): JALR inline lookup (same as benchmark_core).

**Evidence gate:** Dump `-O` for target blocks, count pattern instances,
measure code size and wall time before/after.

---

## Priority 2: Register cache efficiency

**Status: DONE** — expanded from 6 to 8 slots using R8/R9. Block 0xC7DC
(CSV hot loop) improved from 71% to 100% cache hit rate. R8/R9 scratch
usage in JALR/RAS code resolved via pre-flush of dirty cached values.

**Remaining ideas:**
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
20. ~~Peephole MOV-CMP/TEST folding~~ (mov rA,rB; cmp/test rA → substitute rB directly; +35 rewrites on benchmark_core)
21. ~~Emitter: cached registers in branch comparisons~~ (translate_branch_common uses guest_host_reg() directly in CMP/TEST instead of copying to scratch RAX/RCX; -288B code on benchmark_core, -144B on CSV; subsumes most MOV-CMP peephole hits at the source)
22. ~~Redundant bounds check elimination~~ (validates address ranges per base register, skips checks when range already proven safe)
23. ~~Register cache expansion 6→8 slots~~ (added R8/R9; CSV hot block 0xC7DC: 71%→100% cache hit rate; R8/R9 scratch in JALR/RAS pre-flushed before clobber)
24. ~~Direct load/store with cached registers~~ (loads go directly into cached host reg instead of scratch RCX+MOV copy; stores use cached host reg directly; CSV 0xC7DC: 378B→322B, eliminates ~1200 redundant MOVs per CSV run)
25. ~~SETcc and SUB with cached registers~~ (SETcc+MOVZX directly into cached rd instead of going through RAX; SUB rd==rs2 uses NEG+ADD when both operands cached; CSV 0x1678: 396B→390B)
