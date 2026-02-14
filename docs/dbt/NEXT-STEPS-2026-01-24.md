# Next Steps (Stage 3/4 Study)

This note captures recommendations from a full code/spec review so we don't lose them
as we continue iterating on slow32-dbt.

## Status Update (so far)

- Implemented bounded inline probing (4 slots) and pinned lookup_table in R15.
- Added a fixed reg-cache skeleton (4-slot direct-mapped), guarded by `-R`.
- Fixed reg-cache flush semantics (do not clear dirty in flush; multiple paths emit flush code).
- Added per-block reg-cache hit/miss reporting in the "Top 5 hottest blocks" summary.
- Current measurements: reg-cache shows moderate hit rates in hot blocks but small or no runtime gain so far.

## Highest ROI Ideas (ordered)

1) Fix Stage 3 inline-lookup collision behavior

   - Spec expects single-probe, but cache uses linear probing.
   - Result: displaced entries miss inline lookup and fall back to dispatcher.
   - Options:
     - Add a bounded inline probe loop (2-4 slots) and bail after the bound.
     - Or change cache layout to direct-mapped / small set-associative to match single probe.

2) Pin lookup_table in a host register

   - Stage 3 spec says R15 holds lookup_table to avoid loads from cpu state.
   - Inline lookup still loads CPU_LOOKUP_TABLE_OFFSET each time.
   - Pinning R15 trims hot-path loads; consider pinning lookup_mask too if useful.

3) Small register cache (not full allocator)

   - Keep 4-6 host regs live within a block/superblock.
   - Lazy writeback at block end + side exits.
   - Goal: avoid load/op/store per instruction in hot loops.

4) Chaining scalability

   - Current chaining scans all blocks to patch incoming edges (O(n)).
   - Keep per-target incoming lists to make chaining O(#incoming).

5) Instruction count correctness

   - Inline lookup bypasses dispatcher so instr_count can lie.
   - Either increment in generated code or track blocks-executed separately.

## Short list if we only do two

- Bounded inline probing for Stage 3 lookup.
- Small register cache (even a simple one).

## Open Questions / Follow-ups

- Why does reg-cache hit rate not translate into wall-clock gains?
- Should we shrink the cache (2-slot) to reduce overhead, or disable for branchy blocks?
- Does most time still go to loads/stores or branch handling, making cache less relevant?
