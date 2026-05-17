# Stage01 Test Organization

This directory holds the test corpus for the Stage01 Forth-hosted toolchain
(assembler `asm.fth`, archiver `ar.fth`, linker `link.fth`, and C compiler `cc.fth`).

## Conformance Surface (primary teaching + validation material)

These are the files exercised by normal regression and conformance gates
(`run-stages.sh`, `run-subset-conformance.sh`, `run-regression-*.sh`).

- `conformance/baseline/` — small canonical regression cases (`test1.c` … `test9.c`)
- `conformance/subset/` — the core Subset-C litmus tests (13 files). This is the
  minimum contract that `cc.fth` must satisfy.
- `conformance/subset-idioms/` — 62 focused, high-value tests distilled from real
  problems encountered while building later stages (pointer arithmetic shapes,
  preprocessor edge cases, relocation packing, long-branch lowering, archive
  member handling, short-circuit evaluation, etc.). These are the best "why this
  was hard" examples in the entire tree.
- `conformance/known-gaps/` — tracked cases that are intentionally not yet gating
  (currently contains only one implicit-repeat-call repro).

Manifests for these live in `../manifests/` (top-level) so the main runners do not
need path changes.

## Historical Debug / Porting Artifacts

- `historical-debug/` — all the `_bisect*.c` files and the various
  `reloc_bisect*` families that were created while debugging the V2 relocation
  port, long branches, archive shapes, etc.

These are retained for archaeology and to protect against regressions in obscure
corner cases, but they are **not** part of the primary teaching or conformance
surface.

Their manifests live in `historical-debug/manifests/`. The helper
`list-bucket.sh` knows how to find them.

## Running Tests

Normal validation (what must stay green):
- `../run-regression-as.sh test1`
- `../run-regression-ar.sh test3`
- `../run-regression-ld.sh test3`
- `../run-regression-ld.sh archive`
- `../run-subset-conformance.sh`  (uses subset + subset-idioms manifests)

Debug / archaeology:
- `../run-subset-gap-scan.sh`
- `./list-bucket.sh as-bisect | head`

See the top-level `selfhost/stage01/README.md` and `TEACHING.md` for more context.
