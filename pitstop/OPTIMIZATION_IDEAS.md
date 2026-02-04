# SLOW-32 DBT Optimization Opportunities

This document tracks DBT-side opportunities only. LLVM already handles most low-level codegen work, so we only pursue items that measurably reduce DBT overhead or correct DBT-specific behavior.

## Guiding Principles
- Prefer changes that reduce DBT overhead: translation time, dispatcher cost, cache misses, or exit frequency.
- Avoid re-implementing LLVM optimizations unless profiling proves they are still hotspots in current executables.
- Every item below has an evidence gate. If we cannot measure it, it is not ready to implement.

## Evidence Gates (Required Before Implementation)
- Stage comparison: measure `-1/-2/-3/-4` on at least two real workloads.
- Attribution: collect `-p` timing breakdown and block-cache stats.
- Hotspot proof: for any “micro-optimization,” show the affected opcode(s) or block(s) are among the hottest 5.

## Near-Term, DBT-Specific Opportunities

## 1. Reduce Helper/Exit Overhead for Floating-Point Hotspots
Floating-point instructions currently call `dbt_fp_helper` and flush the register cache every time.

Evidence gate:
- Confirm fp ops are in top 5 hottest blocks and that `EXIT_*` counts correlate with fp-heavy code.

Opportunity:
- Implement direct SSE for a *minimal* subset that appears in hot code (`FADD_S`, `FSUB_S`, `FMUL_S`, `FDIV_S`, `FEQ_S/FLT_S/FLE_S`).
- Keep it surgical: only add the ops seen in real programs, not the whole fp ISA.

## 2. Intrinsic Stubs: Pay for the Call Only When It’s Worth It
Memcpy/memset/memmove/strlen already jump to host libc, which is great for large sizes but potentially slow for tiny sizes.

Evidence gate:
- Measure call frequency and typical size distribution for each intrinsic (record sizes for the top 5 hot blocks).

Opportunity:
- Inline tiny sizes (e.g., <= 16 bytes) with direct loads/stores when size is constant at translation time.
- Consider `rep movsb`/`rep stosb` for mid-size ranges if libc call overhead dominates.

## 3. Superblock Policy Tuning (Stage 4)
Stage 4 can regress when superblocks extend across hot back-edges.

Evidence gate:
- Confirm regressions with the same test set and record side-exit stats (`-E`, `-t`, `-D`).

Opportunity:
- Tune default thresholds (`superblock_profile_min_samples`, `superblock_taken_pct_threshold`).
- Consider a “don’t extend across hot back-edges” default when `side_exit_taken/total` is high.

## 4. Register Cache ROI (Stage 4)
The fixed reg-cache can reduce loads/stores, but it adds complexity to exit paths.

Evidence gate:
- For top 5 hot blocks, track `reg_cache_hits/misses` and overall speedup when `-R` is toggled.

Opportunity:
- If reg cache pays off, increase `REG_ALLOC_SLOTS` only if it improves the measured hit rate.
- Otherwise, keep current size and focus on exit-path overhead (deferred exits, flush snapshots).

## 5. Constant Folding Where DBT Actually Sees Constants
LLVM already folds most arithmetic. DBT-side constant folding is only valuable when constants survive into execution (e.g., stateful registers, interpreter-style patterns).

Evidence gate:
- Use a short profiling run to show `reg_constants` sees stable constants in hot blocks.

Opportunity:
- Fold `DIV/REM` only when both operands are constants *at translate time*.
- Avoid signed power-of-two transforms unless the guarded code shows up in hot blocks.

## 6. Dispatch and Cache Efficiency
Dispatch overhead and block-cache misses can dominate small workloads.

Evidence gate:
- Collect `cache_print_stats` and timing breakdown with `-p`.

Opportunity:
- If miss rate is high, evaluate cache sizing or hashing (only if misses drive runtime).
- If dispatch time dominates, consider reducing work in the C dispatcher for Stage 3/4.

## Opportunistic Correctness Improvements (Non-Perf)
These are correctness or safety improvements that could also stabilize performance.
- Add bounds/MMIO/align checks in intrinsic stubs when they are enabled.
- Guard MMIO base calculations against overflow in loader and init.
