# Selfhost V2 Workspace

This tree is the target layout for the 17-stage bootstrap model in `selfhost/docs/BOOTSTRAP-V2.md`.

Current policy:
- Keep existing `selfhost/stage*` paths working while migration is in progress.
- Move one stage at a time with compatibility wrappers/symlinks where needed.
- Update tests/CI scripts in lockstep with each stage move.

Stage directories:
- `stage00` through `stage16` map 1:1 to V2 stage numbers.

Primary tracking docs:
- `selfhost/V2-REORG-PLAN.md`
- `selfhost/V2-MIGRATION-MAP.tsv`
- `selfhost/v2/stage-status.md`

## Ordered Stage Walk

For a clean checkout sanity pass (stage00 -> stage05):

```bash
selfhost/v2/run-stages.sh
```

For faster local loops, you can skip the Stage03 selfhost-kernel regen gate:

```bash
selfhost/v2/run-stages.sh --skip-selfhost-kernel
```

Manual per-stage entry points:
- `stage00`: `make -C selfhost/v2/stage00 && make -C selfhost/v2/stage00 test`
- `stage01`: `selfhost/v2/stage01/run-regression.sh test1`
- `stage02`: `selfhost/v2/stage02/run-regression.sh test3`
- `stage03`: `selfhost/v2/stage03/run-regression.sh test3` and `... archive`
- `stage03` (kernel regen gate): `SELFHOST_EMU=./tools/emulator/slow32-fast selfhost/v2/stage03/run-selfhost-kernel.sh`
- `stage04`: `selfhost/v2/stage04/run-regression.sh`
- `stage05`: `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as --test test1`
