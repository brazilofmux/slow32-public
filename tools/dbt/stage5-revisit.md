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
