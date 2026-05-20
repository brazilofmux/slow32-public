# Selfhost Workspace

This tree implements the multi-cycle bootstrap plan documented in [`docs/BOOTSTRAP.md`](docs/BOOTSTRAP.md). Each `stage*/` directory is one rebuild iteration that produces an entire toolchain (assembler + archiver + linker + compiler + libc) using the previous stage's outputs.

Stage directories at a glance:
- `stage00`: standalone ~800 line emulator (the trust root). Builds via `make` with host `cc`. Optionally runtime-symlinks to `tools/dbt/slow32-dbt` for speed.
- `stage01`: assembler + archiver + linker + C compiler written in Forth (includes kernel fixed-point proof).
- `stage02`: the same four tools (assembler, archiver, linker, compiler) re-authored in Subset C, still hosted by the Forth toolchain. After this stage, all Forth tools are retired.
- `stage03`: first canonical `cc.s32x` compiler + tools (as, ar, ld) + libc/runtime. Self-sufficient toolchain. Compiled by stage02 cc-min.
- `stage04`: canonical `cc.s32x` compiler with Ragel lexer + recursive-descent parser + tools + libc/runtime. Compiled by stage03.
- `stage05`: code-quality cycle introducing HIR/SSA, BURG instruction selection, and graph-coloring register allocation. Compiled by stage04.
- `stage06`: next selfhost cycle seeded from stage05; supports a `--fixed-point` gate that enforces gen2 == gen3.
- `stage07`: same toolchain shape as stage06 plus richer headers (`assert.h`, `math.h`, `signal.h`, `stdio.h`, `time.h`, `ucontext.h`, `sys/*`) so non-trivial SLOW-32 programs (DBT, full emulator) build inside SLOW-32.
- `stage08`: fork of stage07; the active head for new language features (e.g. bitfields) and future evolution. Cross-compilers now pull their frontend from here.

Sibling cross-compiler trees (independent of the numbered cycle):
- `stage08-cross-x64`: C → x86-64 ELF. Produces `cc-x64`, `ld-x64`, `ar-x64`, `libc_x64.a`, `s32fast-hir` (SLOW-32 fast emulator), and `dbt-x64` (SLOW-32 dynamic binary translator). Frontend symlinked from `../stage08/`.
- `stage08-cross-a64`: C → AArch64 ELF. AArch64 sibling; frontend symlinked from `../stage08/`. Produces `cc-a64`, `ld-a64`, `ar-a64`, `libc_a64.a`, `s32fast-hir`, and `dbt-a64`.

Reference docs:
- [`docs/BOOTSTRAP.md`](docs/BOOTSTRAP.md) — canonical bootstrap roadmap (V2 stages 0–16, sibling cross-compiler track, trust model).
- [`TOOL-NAMING.md`](TOOL-NAMING.md) — tool-name conventions across the stages.
- [`ISSUES.md`](ISSUES.md) — code-review findings (closed and active) across the toolchain.

## Ordered Stage Walk

For a clean checkout sanity pass:

```bash
selfhost/run-stages.sh
```

`run-stages.sh` walks `stage00` → `stage08` (use `--from`/`--to` to scope, e.g. `--to stage04`). The cross-compiler trees have their own entry points (below). For faster local loops, you can skip the selfhost-kernel regen gate:

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
- `stage03` (compiler `cc.s32x`): `selfhost/stage03/run-spike.sh --emu ./tools/emulator/slow32-fast`
- `stage04` (compiler `cc.s32x`): `selfhost/stage04/run-tests.sh --emu ./tools/emulator/slow32-fast`
- `stage05` (optimized toolchain): `selfhost/stage05/run-tests.sh --emu ./tools/emulator/slow32-fast`
- `stage06` (next-cycle toolchain): `selfhost/stage06/run-tests.sh --emu ./tools/emulator/slow32-fast` (add `--fixed-point` to gate gen2 == gen3)
- `stage07` (full-libc toolchain): `selfhost/stage07/run-tests.sh --emu ./tools/emulator/slow32-fast` (add `--fixed-point` to gate gen2 == gen3)
- `stage08` (active head, fork of stage07): `selfhost/stage08/run-tests.sh --emu ./tools/emulator/slow32-fast` (add `--fixed-point` to gate gen2 == gen3)
- `stage08-cross-x64` (x86-64 cross): `make -C selfhost/stage08-cross-x64 && make -C selfhost/stage08-cross-x64 test` (then `make bench` to time `s32fast-hir`)
- `stage08-cross-a64` (AArch64 cross): `make -C selfhost/stage08-cross-a64 && make -C selfhost/stage08-cross-a64 test`

ABI conformance entry points:
- all supported stages: `selfhost/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt` (currently scoped to stages 03–06)
- stage03 only: `selfhost/stage03/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
- stage04 only: `selfhost/stage04/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
- stage05 only: `selfhost/stage05/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
- stage06 only: `selfhost/stage06/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
- stage07 only: `selfhost/stage07/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
- stage08 only: `selfhost/stage08/run-abi-conformance.sh --emu ./tools/dbt/slow32-dbt`
