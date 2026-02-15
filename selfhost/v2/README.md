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
