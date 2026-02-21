# BURG Migration Audit (Stage05)

Goal: move codegen-shape rewrites out of `hir_opt.h` and into BURG selection so we stop generating fixup patterns and reduce peephole debt.

## Scope

- Source of current rewrites: `selfhost/stage05/hir_opt.h:123`
- BURG tables/select: `selfhost/stage05/hir_burg.h:103`
- BURG-dispatched emission: `selfhost/stage05/hir_codegen.h:463`

## Rule Matrix

| Rule family | Current location | Disposition | BURG target |
|---|---|---|---|
| `x + c` / `c + x` -> `HI_ADDI` | `selfhost/stage05/hir_opt.h:242`, `selfhost/stage05/hir_opt.h:314` | `migrate-to-BURG` | Add `HI_ADD(BG_REG,BG_IMM)` and `HI_ADD(BG_IMM,BG_REG)` patterns and emit as `addi` in `hcg_inst` |
| `x - c` -> `HI_ADDI x, -c` | `selfhost/stage05/hir_opt.h:249` | `migrate-to-BURG` | Add `HI_SUB(BG_REG,BG_IMM)` pattern and emit `addi`/`li+add` in `hcg_inst` |
| `addi x, 0` -> `copy` | `selfhost/stage05/hir_opt.h:413` | `migrate-to-BURG` | Keep HIR as-is; in BURG/codegen emit move/skip for zero-immediate add |
| `addi(addi x,c1),c2` combine | `selfhost/stage05/hir_opt.h:421` | `migrate-later` | Add BURG cost/select support for reg+imm chain collapse (not just `FADDR`) |
| `addi(iconst,c)` -> `iconst` | `selfhost/stage05/hir_opt.h:427` | `keep-IR` | Semantic fold; best done before ISel |
| `ALLOCA/ADDI(ALLOCA)` frame offset combine | BURG already | `already-in-BURG` | `BG_FADDR` + `bg_foff` (`selfhost/stage05/hir_burg.h:308`) |
| `LOAD/STORE` addr-form folding (`faddr/saddr`) | BURG already | `already-in-BURG` | `BG_FADDR/BG_SADDR` patterns (`selfhost/stage05/hir_burg.h:120`) |
| algebraic identities (`x*1`, `x&-1`, `x^x`, etc.) | `selfhost/stage05/hir_opt.h:197` | `keep-IR` | Canonicalization/constant propagation, not codegen-shape |
| constant-fold binops/unary (`iconst` trees) | `selfhost/stage05/hir_opt.h:148`, `selfhost/stage05/hir_opt.h:389` | `keep-IR` | Reduces IR size and enables CFG simplification |
| `BRC(iconst)` -> `BR` | `selfhost/stage05/hir_opt.h:436` | `keep-IR` | CFG simplification should happen pre-ISel |

## Batch Plan

### Batch A (safe, high-value)

1. Add BURG support for immediate forms:
   - `HI_ADD(BG_REG,BG_IMM)`
   - `HI_ADD(BG_IMM,BG_REG)`
   - `HI_SUB(BG_REG,BG_IMM)`
2. Extend `hcg_inst` emission for these patterns to produce `addi`/`li+add`.
3. Remove overlapping rewrites in `hir_opt.h`:
   - `x + c` / `c + x` -> `HI_ADDI`
   - `x - c` -> `HI_ADDI`
4. Keep semantic folds and branch simplification unchanged.
Status: completed in `c5ff8ba`.

### Batch B

1. Add BURG-side handling for `HI_ADDI` chains in general-reg form.
2. Remove `addi(addi x,c1),c2` rewrite from `hir_opt.h`.
Status: completed (codegen-side chain fold + peephole removal).

### Batch C

1. Decide fate of `addi x,0`:
   - either keep as IR canonicalization to `HI_COPY`, or
   - keep in HIR and treat as zero-cost move in ISel/emission.
2. Pick one behavior and enforce consistently (avoid duplicate logic).
Status: completed with codegen-owned policy (`HI_ADDI` kept in HIR; zero-offset
handled in emission, optimizer rewrite removed).

## Test gates per batch

Run all after each batch:

1. `bash selfhost/stage05/run-tests.sh`
2. Fixed-point proof (`gen1 == gen2`) in the normal stage05 workflow
3. ABI conformance suite (existing stage05 ABI script)
