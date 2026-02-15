# V2 Reorg Plan (Code, Tests, Infrastructure)

This plan executes the 17-stage model in `selfhost/docs/BOOTSTRAP-V2.md` by migrating repository layout in low-risk slices.

## Principles

- Preserve working pipelines at every step.
- Move one stage boundary at a time.
- Keep compatibility shims until scripts and docs are fully switched.
- Require green validation gates before and after each move.

## Phases

## Phase 1: Scaffolding and Mapping (completed)

Deliverables:
- `selfhost/v2/stage00..stage16/` scaffold created.
- Stage-to-path migration map committed (`selfhost/V2-MIGRATION-MAP.tsv`).
- Execution plan committed (this file).

Validation gate:
- No runtime behavior changes.
- Existing stage2/stage3/stage4 scripts still runnable unchanged.

## Phase 2: Test/Validation Buckets

Actions:
- Introduce stable test buckets under `selfhost/v2/stageXX/tests/`.
- Split stage4 tests into:
  - baseline regression (`test1..test9`)
  - bisect/repro corpus (`as_bisect*`, `ar_bisect*`)
  - validation spikes (`s32-as.c`, `s32-ar.c`, `slow32dump.c`, `slow32dis.c`)
- Add wrapper scripts that reference new locations but keep old entry points.
- Make core stage scripts path-override aware so they run from both legacy and v2 trees.

Progress (2026-02-15):

- Added stage04 bucket manifests under `selfhost/v2/stage04/tests/manifests/`.
- Added bucket lister utility `selfhost/v2/stage04/tests/list-bucket.sh`.
- Promoted full stage04 assembler bisect corpus (`as_bisect1..22,27,28`) to tracked fixtures.
- Added path-override-aware runners:
  - `selfhost/stage2/run-regression.sh`
  - `selfhost/stage3/run-regression.sh`

Validation gate:
- `selfhost/stage4/run-regression.sh` passes unchanged.
- New wrapper entry points pass same checks.

## Phase 3: Stage 0-4 Code Relocation with Compatibility

Actions:
- Move stage code to `selfhost/v2/stage00..stage04`.
- Leave compatibility stubs/symlinks in legacy `selfhost/stage*` paths.
- Update docs and scripts to prefer `selfhost/v2/...` paths.

Validation gate:
- Stage4 regression green using both legacy and v2 paths.
- Stage4 validation `s32-as.c` pipeline green.

Progress (2026-02-15):

- Stage4 code has been relocated to `selfhost/v2/stage04/`.
- Legacy `selfhost/stage4/` now provides compatibility symlinks to the v2 location.
- Regression verified from both entry points:
  - `selfhost/stage4/run-regression.sh`
  - `selfhost/v2/stage04/run-regression.sh`
- Stage2 relocated to `selfhost/v2/stage01/` with legacy compat symlinks in `selfhost/stage2/`.
- Stage3 relocated to `selfhost/v2/stage03/` with legacy compat symlinks in `selfhost/stage3/`.
- Stage0 tracked sources relocated to `selfhost/v2/stage00/` (canonical location).
- Stage2 archiver split to canonical `selfhost/v2/stage02/ar.fth` with compatibility links from stage3 paths.

## Phase 4: Stage 5/6 Integration Tracks

Actions:
- Wire `s32-as.c` and `s32-ar.c` into optional progressive pipeline scripts.
- Add A/B pipeline mode:
  - baseline (Forth assembler/linker)
  - progressive (C assembler/archiver where available)

Validation gate:
- Baseline mode remains green.
- Progressive mode green for implemented replacement stages.

Progress (2026-02-15):

- Added `selfhost/v2/stage05/run-pipeline.sh` A/B harness with modes:
  - `baseline`
  - `progressive-as`
  - `progressive-as-ar`
- Added `selfhost/v2/stage05/README.md` usage notes.
- Updated Stage4 regression defaults to canonical v2 stage paths (`stage01`/`stage03`).

## Phase 5: Infra/CI and Cleanup

Actions:
- Add CI targets for v2 stage gates.
- Remove legacy compatibility paths only after two green cycles.
- Finalize directory mapping in docs and remove transitional notes.

Validation gate:
- CI green on v2 stage gates.
- No references to removed legacy paths.

## Immediate Next Tasks

1. Validate `stage05/run-pipeline.sh` on both baseline and progressive modes, then freeze initial gate targets.
2. Add initial CI stage-gate checks for relocated canonical paths under `selfhost/v2/`.
3. Add stage04 bucket health checks that assert manifest/file-set parity for bisect corpora.
