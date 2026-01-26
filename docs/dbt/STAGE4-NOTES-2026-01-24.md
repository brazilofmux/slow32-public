# Stage 4 DBT Notes (2026-01-24)

Context
- Goal for Stage 4 is study/visibility, not speed.
- MMIO is NOT supported in slow32-dbt yet; other emulators support MMIO.
- Stage 3 remains best-performing DBT mode so far; Stage 4 can regress when superblock extension is ill-advised.

Baseline performance (user-provided summary, best of 3 runs)
- Native -O2: fib 8 ms, prime 4 ms
- Native -O0: fib 62 ms, prime 4 ms
- slow32-dbt -3: fib 126 ms, prime 94 ms
- slow32-dbt -4: fib 276 ms, prime 293 ms (regression)
- QEMU TCG: fib 205 ms, prime 234 ms
- slow32-fast: fib 302 ms, prime 479 ms
- slow32 (interp): fib 1960 ms, prime 3171 ms
Key takeaway: Stage 3 DBT is fastest; Stage 4 regresses when superblocks expand across hot back-edges.

What we learned from Stage 4 diagnostics
- Superblock averages are small and consistent (about 14-16 guest insts, 1.7-1.9 exits, ~22-23 host bytes/guest inst).
- The prime offender shows a superblock that includes the loop body AND still exits on a hot back-edge.
  - Example: SB 0x00003944-0x00003960, back_edges=1, execs=13,289,383, taken 489,219 / total 13,289,384.
  - This is a classic "bad superblock": larger host code without eliminating the hot branch.

Annotated offender (prime)
- Stage 3 block at 0x00003938 (16 bytes) ends at BEQ.
- Stage 4 superblock at 0x00003934 (48 bytes) includes the loop body and the back-edge BLTU.
- Host code is ~3x larger, but still exits frequently -> regression.

New diagnostics added
- Superblock summary stats: avg insts, avg exits, host bytes per inst.
- Per-superblock diagnostics line: guest range, exit count, back-edge count, max taken sample, exec count.
- "avoid(back_edge+hot)" annotation when back_edge && execs > 1000.

Study-only guard
- Flag: -B (study-only: avoid extending across back-edges).
- Behavior: if a would-be superblock contains a back-edge, retranslate that block without extension.
- Result on bench-prime: time drops to ~0.093s, avoids the hot offender superblock.

Insights
- Register pressure does not appear to be the main limiter in current hot loops.
- Current limiters: exit overhead, memory traffic (loads/stores per guest op), and superblock expansion that does not reduce exits.

Hypothesis / next idea
- Larger superblocks increase opportunity for peephole optimization.
- Peephole may be a better Stage 5/6 path than more aggressive superblock extension without stronger analysis.

Relevant commits
- 06f27e3 Add superblock summary stats
- 8c1d70f Add per-superblock diagnostics
- d5deefa Flag hot back-edge superblocks
- 356cb42 Add back-edge guard for study

Suggested next steps (study)
- Use diagnostics to identify superblocks with back_edges + high execs; avoid or inspect.
- Compare -4 vs -4 -B across benchmarks to quantify regression sources.
- If pursuing performance later: explore peephole passes on superblocks before reworking allocation/SSA.

## Follow-up: micro-opts + tracing (2026-01-24)
- Added emit tracing (env `SLOW32_DBT_EMIT_TRACE=1`, optional `SLOW32_DBT_EMIT_TRACE_PC=0x...`) to map hot-block x86 to codegen sites.
- Verified hot block 0x3944 is dominated by guest_load/guest_store plus real ALU ops; exit bookkeeping is no longer the main cost.
- Pushed immediate stores into codegen (exit PC/reason/info and constant guest reg writes) and replaced zero compares with `test`.
- Benchmarks did not materially improve; the remaining cost is structural (guest reg traffic + superblock control flow).
- Side-exit bookkeeping is now behind `-E` (study-only). Default runs skip that overhead; `-t` still enables it for profiling.

## Follow-up: inline lookup + reg-cache experiments (2026-01-24)
- Implemented bounded inline probing (4 slots) to align with linear-probing cache; also pinned `lookup_table` in R15.
- Added fixed reg-cache skeleton (4-slot direct-mapped), guarded by `-R`.
- Fixed reg-cache flush semantics: do not clear dirty in `flush` because multiple code paths emit flush code.
- Added per-block reg-cache hit/miss reporting in the “Top 5 hottest blocks” stats.
- Early results: reg-cache shows moderate hit rates in hot blocks but small or no wall-clock gains yet.

Conclusion: Stage 4 insights are exhausted for now; any next gains likely need structural changes (reg caching / writeback suppression / different block strategy), or returning to Stage 3 / Stage 4 -B for production readiness.

## Conclusion (mothballing DBT for now)
- The study work shows potential gains, but unlocking them reliably likely requires richer analysis (dataflow, liveness, cost modeling) that approaches a “mini-LLVM.”
- Without that scaffolding, improvements are fragile and workload-dependent.
- Next practical work (if/when resumed): add YIELD/MMIO support so DBT can run real workloads, but note that DBT is riskier than QEMU TCG for semi-production use because it emits native code.
