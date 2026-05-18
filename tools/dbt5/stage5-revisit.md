# Stage5 Revisit List (Jupiter Path)

1. Branch/flags discipline:
- Keep compare + branch tightly coupled in emission shapes.
- Avoid "set flags here, branch much later" patterns unless protected from flag clobbers.
- Continue preferring fused compare-branch terminals and side-exit forms.

2. BURG + peephole split:
- BURG should own structural selection (tree/superblock shapes).
- Peephole should clean residual ISA details after emission.
- Track candidates where peephole can be migrated into richer BURG patterns.

3. Terminal ownership gaps:
- Audit remaining `translate_*` terminal delegation from Stage5.
- Convert one terminal family at a time behind existing gate/sweep.

4. Native memory checks:
- Reconcile Stage5 native load/store checks with advanced Stage4 checks
  (range elimination, dynamic-size behavior), without delegating emission.

5. Regalloc maturity:
- Stage5 still uses cache-era host slot concepts; continue moving toward
  explicit region RA + spill policy independent of Stage4 assumptions.

6. Measurement hooks:
- Keep Stage4-vs-Stage5 parity sweep as required gate.
- Add stage5-native counters for branch family ownership and flag-sensitive shapes.

## Hybrid (AArch64) Progress (May 2026)
- Lifter (stage5_lift_superblock + CFG) is now wired into translate_a64.c under the existing `-5` / stage5_*_enabled flags.
- On benchmark_core.s32x (the key perf workload): 90/90 blocks lifted successfully, 0 too-large/unsupported. Lift cost < 20 µs per block.
- First Option A investment: after successful lift we seed `ctx->backedge_targets[]` / `has_backedge` from the lifter's CFG *before* the heuristic prescan. This feeds the proven fast in-block back-edge machinery (direct relative jumps, targeted flushes) with strictly richer data than the linear negative-imm scan.
- Result on the same binary: 14 back-edges seeded from the lifter. Default (no `-5`) path is 100 % untouched.
- This is the "proof" that the lifter the user previously validated with LLVM opt -O2 is already delivering value on the target platform by improving the current fastest path when the advanced mode is requested.
- Next natural steps (real long-term investment): grow a thin A64 emission path from the lifted region (or from MIR) that can eventually own whole blocks the way x86 Stage 5 does today, while continuing to use the same CFG/SSA for both "seed the fast path" and "real compiler backend" modes.
