# Selfhost V2 Workspace

This tree tracks the multi-cycle bootstrap plan captured in `selfhost/stage-status.md` (derived from `selfhost/docs/BOOTSTRAP.md`).

Current policy:
- Keep existing `selfhost/stage*` paths working while migration is in progress.
- Move one stage at a time with compatibility wrappers/symlinks where needed.
- Update tests/CI scripts in lockstep with each stage move.

Stage directories:
- `stage00` through `stage05` map to the current V2 stage numbers.

Stage cycles at a glance:
- `stage00`: standalone ~800 line emulator that every other stage relies on.
- `stage01`: assembler + archiver + linker + C compiler written in Forth (includes kernel fixed-point proof).
- `stage02`: the same four tools (assembler, archiver, linker, compiler) now authored in Subset C but still hosted by the Forth toolchain. After this stage, all Forth tools are retired.
- `stage03`: s32-cc compiler + tools (as, ar, ld) + libc/runtime. Self-sufficient toolchain. Compiled by stage02 cc-min, tools compiled by s32-cc. First Forth-free stage.
- `stage04`: s12cc AST-based compiler (Ragel lexer + recursive-descent parser) + tools + libc/runtime. Language capability stage. Compiled by stage03 s32-cc.
- `stage05`: code quality — optimized s12cc toolchain (compiler + AS + AR + LD + libc). Compiled by stage04.

Primary tracking docs:
- `selfhost/V2-REORG-PLAN.md`
- `selfhost/V2-MIGRATION-MAP.tsv`
- `selfhost/stage-status.md`

## Ordered Stage Walk

For a clean checkout sanity pass (stage00 -> stage03):

```bash
selfhost/run-stages.sh
```

For faster local loops, you can skip the selfhost-kernel regen gate:

```bash
selfhost/run-stages.sh --skip-selfhost-kernel
```

Manual per-stage entry points:
- `stage00`: `make -C selfhost/stage00 && make -C selfhost/stage00 test`
- `stage01` (assembler): `selfhost/stage01/run-regression-as.sh test1`
- `stage01` (archiver): `selfhost/stage01/run-regression-ar.sh test3`
- `stage01` (linker): `selfhost/stage01/run-regression-ld.sh test3` and `... archive`
- `stage01` (kernel regen gate): `SELFHOST_EMU=./tools/emulator/slow32-fast selfhost/stage01/run-selfhost-kernel.sh`
- `stage01` (compiler): `selfhost/stage01/run-regression-cc.sh`
- `stage02` (assembler): `selfhost/stage02/run-pipeline.sh --mode progressive-as --test test1`
- `stage02` (archiver): `selfhost/stage02/run-pipeline.sh --mode stage6-ar-smoke` and `... stage6-ar-rc-smoke` and `... stage6-ar-tx-smoke`
- `stage02` (linker): `selfhost/stage02/run-spike-ld.sh`
- `stage02` (compiler): `selfhost/stage02/run-regression-cc-min.sh --emu ./tools/emulator/slow32-fast`
- `stage03` (s32-cc compiler): `selfhost/stage03/run-spike.sh --emu ./tools/emulator/slow32-fast`
- `stage04` (s12cc compiler): `selfhost/stage04/run-tests.sh --emu ./tools/emulator/slow32-fast`
- `stage05` (optimized toolchain): `selfhost/stage05/run-tests.sh --emu ./tools/emulator/slow32-fast`
