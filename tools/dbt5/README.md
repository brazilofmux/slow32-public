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

## Current status (as of the fork)

- Full lifter + CFG + SSA + MIR + BURG + LIR + RA pipeline is present and compiles.
- On x86-64 hosts the existing `stage5_codegen.c` (LIR → native x86-64) can be used.
- On AArch64 (the primary development host at the time of the fork) there is **no native codegen yet**. `dbt5` can run the complete analysis pipeline and fall back to the shadow interpreter for execution. This is the correct place to write `stage5_codegen_a64.c` (and its supporting thin runtime context) from scratch.

## Building

```bash
cd tools/dbt5
make
./slow32-dbt5 some.s32x --lift-only     # analysis only
./slow32-dbt5 some.s32x                 # run via shadow fallback (AArch64)
```

## Next real work

The highest-leverage thing to do inside this tree is to design and implement the missing AArch64 native emission path:

1. Define a small `stage5_cg_a64_ctx_t` (or reuse/extend the concepts from the x86 codegen) that only contains what a pure code generator needs.
2. Write `stage5_codegen_a64.c` that walks LIR (or even the lifted region for a narrow pilot) and emits using only `emit_*` primitives from `emit_a64.c`.
3. Wire a simple execution loop that installs the emitted code into a block cache and runs it.

All of that work belongs here, not in the production `dbt` tree.

## Relationship to the old "narrow owning" pilot

The old pilot code that lived in `translate_a64.c` (the `stage5_replay_narrow_*`, `narrow_ownership_enabled` flag, lifter-driven prologue seeding, etc.) has been left behind in the production tree. It was a hybrid that ultimately still drove the Stage 4 helpers. The real experiment continues here without that scaffolding.
