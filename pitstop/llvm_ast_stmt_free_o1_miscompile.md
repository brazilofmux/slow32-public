# Suspected LLVM Miscompile: `sbasic` `stmt_free` at `-O1`

## Summary

On Intel host, `sbasic` built with the SLOW-32 LLVM pipeline at default `-O1`
showed allocator faults in both interpreter and DBT runs. The same sources built
at `-O0` passed cleanly. Localization points to `sbasic/src/ast.c`, specifically
`stmt_free`.

This appears to be outside DBT and likely codegen/optimization-related in the
SLOW-32 LLVM backend or pipeline.

## Symptoms

Strict triage run:

```bash
cd sbasic/tests
STRICT_FAULTS=1 FILTER_FAULTS=0 EMU=../../tools/emulator/slow32-fast bash ./run-tests.sh
```

At `-O1` (before workaround), failures included:
- `arrays`: `Error: Read out of bounds at 0xfffffff9`
- `dataread`: `Error: Read out of bounds at 0x4154413c`

Cross-emulator repro:
- `slow32`: memory fault at `PC=0x0001A1E8`
- `slow32-fast`: same failing site (`HALT at PC 0x0001a1ec`)
- `slow32-dbt`: `DBT: Memory fault at PC=0x0001A1E8`

Disassembly at fault site:
- `free` entry in `sbasic/sbasic.s32x`
- faulting instruction: `ldw r7, r3+-8` at `0x0001A1E8`

## A/B Localization

1. Build full `sbasic` with `OPT=-O0`: strict suite passes `29/29`.
2. Build full `sbasic` with default `-O1`: strict suite fails (`arrays`, `dataread`).
3. Mixed-link test:
   - all objects from `-O1`, except `src/ast.s32o` from `-O0`
   - strict suite passes `29/29`.

This isolates the regression to `ast.c` codegen under `-O1`.

## Candidate Trigger Region

`sbasic/src/ast.c` function:
- `stmt_free`

Potentially sensitive pattern:
- large tagged-union switch
- many case arms with variant-specific pointer frees
- linked-list recursion/iteration patterns in nearby AST free logic

## Current Project Workaround

Targeted workaround applied in project tree:

```c
#if defined(__clang__)
__attribute__((optnone))
#endif
void stmt_free(stmt_t *s) { ... }
```

This keeps global `-O1` while preventing the failure in this hotspot.

## What to Investigate in `~/llvm-project`

1. Reduce `stmt_free` IR to minimal reproducer from `ast.c`.
2. Compare `-O0` vs `-O1` generated SLOW-32 assembly for `stmt_free`.
3. Check for incorrect transform around:
   - union field accesses in switch arms
   - dead-store elimination and alias assumptions
   - control-flow simplification across case blocks
4. Run llc with debug/verify passes on reduced IR.
5. If reproducible, file backend bug with minimized IR and asm diff.

## Follow-up Check: `-fno-jump-tables` A/B

Requested check was run explicitly on Intel with three variants of `ast.c` codegen:

1. `-O1` (normal)
2. `-O1 -fno-jump-tables`
3. current tree workaround/reference build

### Results

- **Current fixed `ast.c`** (semantic cleanup fix present):
  - `-O1`: strict `sbasic` suite `29/29`
  - `-O1 -fno-jump-tables`: strict `sbasic` suite `29/29`

- **Pre-fix `ast.c`** (from commit `d4ea81d`, no workaround):
  - `-O1`: strict suite `27/29` (same two failures)
  - `-O1 -fno-jump-tables`: strict suite `27/29` (same two failures)

### Interpretation

Disabling jump tables does **not** change the failing behavior in the pre-fix case.
This weakens the hypothesis that switch lowering/jump-table generation is the
primary fault mechanism for the observed failures.

The concrete semantic bug in `stmt_free` was required to restore stability.
If a backend issue remains, it is likely elsewhere (not jump-table-only).

## Notes

- Native host ASan run (x86) also pointed at `stmt_free` cleanup path before
  workaround, reinforcing a cleanup-path corruption source.
- Because the same failure signature occurred in interpreter and DBT, DBT was
  ruled out as primary cause.
