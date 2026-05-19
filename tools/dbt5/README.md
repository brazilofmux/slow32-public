# tools/dbt5 — Stage 5 only (clean-room experiment)

This is the independent home for the full Stage 5 DBT work.

## Purpose

Stage 4 (the production path in `../dbt/`) is an extremely strong per-instruction + superblock + register-cache translator that leverages excellent LLVM-generated SLOW-32 code. It forms both a performance floor and a development gravity well.

Stage 5 is the deliberate attempt to escape that well:
- Lift guest code to a clean IR (already done, arch-neutral)
- Build SSA form + value numbering
- Tree-matching instruction selection (BURG)
- Architecture-neutral MIR → target LIR
- Real register allocation (not a 7/8-slot callee-saved cache)
- Direct emission using only the raw `emit_a64` / `emit_x64` primitives

Because every previous attempt to grow "real" Stage 5 emission on AArch64 ended up calling the mature `translate_*` helpers (and therefore the whole Stage 4 machine), we have physically forked the tree.

## Strict separation rule

**No source file under `tools/dbt5/` may ever `#include` or call anything defined in `../dbt/translate*.c` or `../dbt/translate.h`.**

The only allowed shared pieces are the obvious infrastructure:
- raw emitters (`emit_a64.*`, `emit_x64.*`)
- block cache
- cpu_state, dbt_limits
- shadow interpreter (for validation / fallback)
- the S32X loader and common format headers

If you feel the temptation to call `translate_add`, `translate_ldw`, `reg_alloc_*`, or any of the fusion/peephole logic from the old translator — stop. That is exactly the gravity well this directory exists to escape.

## Current status (2026-05-18)

The full lifter + CFG + SSA + MIR + BURG + LIR + RA pipeline is wired
into a working AArch64 emission path with a translate-on-miss
dispatcher and first-execution differential validation. See
`ISSUES.md` for the live tracker; commit messages reference its IDs
directly (e.g. `A8`, `G2+G3`, `E4`).

Done so far (categories from `ISSUES.md`):

- **A1–A10** — all known correctness bugs in `stage5_codegen_a64.c`
  (value_to_host init, CMP/SETCC materialization, side-exit cond +
  target, terminal handling, negative immediates, silent default skip,
  prologue per-slot guest-reg selection, per-guest-reg writeback +
  alias flush, CBZ/CBNZ + 14-slot RA pool, internal-branch fixup cap).
- **B1–B4** — wiring (cache_init pool, `DBT_JIT_MMAP_FLAGS` for Apple
  W^X, real shadow execution, asm clobber hygiene).
- **C1–C6** — cleanup pass (dead stubs, stale comments, LIR dump,
  `LIR_OP_CALL` dst_v, unused-warning hygiene).
- **D1–D4** — LIR x86-shaped baggage removed for the a64 path, 14-slot
  host RA pool replaces the old 8-slot Stage-4 ceiling, CFG extractor +
  SSA threading (no more re-lifting per block), explicit `cg_bail`
  contract for safe shadow fallback.
- **E1–E4** — precise prologue from lifter `live_in_mask` + earliest
  RA interval per incoming gpr, logical-immediate constant
  materialization, RA spill bias for live-across-edge / loop carriers,
  first-execution shadow differential validation with eviction via
  `cache_remove()` (`S5_E4=1`).
- **G1–G3** — translate-on-miss dispatcher loop, JAL/JALR call-graph
  exit semantics, distinguished HALT/YIELD/DEBUG exits.

On x86-64 hosts the existing `stage5_codegen.c` (LIR → native x86-64)
still works; the clean-room x86-64 port (`stage5_codegen_x64.{c,h}`)
remains a placeholder for future Intel-hardware work — see below.

This is **not** wired into the production `slow32-dbt` — Stage 4 in
`../dbt/` remains the production translator. dbt5 is the experiment.

## Building

```bash
cd tools/dbt5
make
./slow32-dbt5 some.s32x --lift-only     # analysis only
./slow32-dbt5 some.s32x                 # run via shadow fallback (default)
S5_EMIT_A64=1 ./slow32-dbt5 some.s32x   # try native a64 emit on the dispatcher path
S5_E4=1 S5_EMIT_A64=1 ./slow32-dbt5 some.s32x  # + first-exec differential w/ eviction
```

## Next real work

`ISSUES.md` is the live punch list. Roughly:

- Pilot scope is done — emit, wire, execute, validate are all in place.
- Remaining items are mostly **OPPORTUNITY** / **DESIGN**: widening the
  fast path, broadening lowering coverage past the pilot's surface,
  and eventually weighing whether dbt5 graduates into a production
  path or stays an experiment.
- See ISSUES.md section F ("Suggested order of attack") for the
  current next-step recommendation.

## x86-64 Migration Plan (Placeholders Added)

The original working x86-64 Stage 5 emitter lives in the legacy
`stage5_codegen.c` (still coupled to the old translator runtime).

As of this commit we have added:

- `stage5_codegen_x64.h` + `stage5_codegen_x64.c` (clean-room target)
- These are currently stubs with clear migration comments.

The intent is that when someone is on an x86-64 host (or an x86-64 agent
is working), they can finish the real independent port into these new
files.  Once that is solid, the old coupled code can be removed from the
production `./tools/dbt` tree.

This keeps the clean-room boundary while still making the migration path
visible in the repository.

## Relationship to the old "narrow owning" pilot

The old pilot code that lived in `translate_a64.c` (the `stage5_replay_narrow_*`, `narrow_ownership_enabled` flag, lifter-driven prologue seeding, etc.) has been left behind in the production tree. It was a hybrid that ultimately still drove the Stage 4 helpers. The real experiment continues here without that scaffolding.
