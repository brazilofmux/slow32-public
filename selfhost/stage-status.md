# V2 Stage Status

This file is the quick orientation map for bootstrap work.

| Stage | Goal | Entry | Current State | Next Blocker / Step |
|---|---|---|---|---|
| `stage00` | Minimal bootstrap emulator (`.s32x` runner) | `make -C selfhost/stage00 && make -C selfhost/stage00 test` | completed | none |
| `stage01` | Forth assembler (`.s -> .s32o`) | `selfhost/stage01/run-regression.sh test1` | completed | none |
| `stage02` | Forth archiver (`.s32o -> .s32a`) | `selfhost/stage02/run-regression.sh test3` | completed | expand corpus beyond `test3` fixture path |
| `stage03` | Forth linker (`.s32o + .s32a -> .s32x`) | `selfhost/stage03/run-regression.sh test3` and `... archive`; `SELFHOST_EMU=./tools/emulator/slow32-fast selfhost/stage03/run-selfhost-kernel.sh` | completed (kernel regen gate enabled in stage walk) | add stricter output equivalence checks (optional) |
| `stage04` | Forth-hosted subset C compiler (`.c -> .s`) | `selfhost/stage04/run-regression.sh` | completed (stage gate) | maintain stability while Stage5/6 replace tools |
| `stage05` | Subset-C assembler replacement (`.s -> .s32o`) | `selfhost/stage05/run-pipeline.sh --mode progressive-as --test test1` | in progress (green on `test1..test3`) | extend green set to full stage04 regression corpus |
| `stage06` | Subset-C archiver replacement (`.s32o -> .s32a`) | `selfhost/stage05/run-pipeline.sh --mode stage6-ar-smoke` and `... stage6-ar-rc-smoke` and `... stage6-ar-tx-smoke` and `... stage6-ar-d-smoke` | in progress (smoke gate green on `c/rc/t/x/d`) | widen coverage to `m/v/p` parity and archive-scan parity (`cs`) |
| `stage07` | Subset-C linker replacement (`.s32o + .s32a -> .s32x`) | `selfhost/stage07/run-spike.sh --with-reloc-spike` | in progress (single-object + bounded `REL_32/HI20/LO12` spike wired) | widen to branch/jal relocations + archive member resolution |
| `stage08` | Subset-C transition gate (archiver parity + `cc-min` spike) | `selfhost/stage08/run-regression.sh` | in progress (archiver parity + first compiler-in-C spike green) | expand `cc-min` toward real parser/codegen (`cc.c`) while preserving gate |
| `stage09` | Subset-C fixed-point proof (Gen2==Gen3) | `selfhost/stage09/` | not started | define reproducible fixed-point harness |
| `stage10` | Expanded/full emulator track | `selfhost/stage10/` | not started | choose scope beyond stage00 minimum emulator |
| `stage11` | Full C compiler feature expansion | `selfhost/stage11/` | not started | language/runtime feature roadmap |
| `stage12` | Rebuild full toolchain with full compiler | `selfhost/stage12/` | not started | stage11 completion |
| `stage13` | Full C fixed-point proof | `selfhost/stage13/` | not started | stage12 reproducibility harness |
| `stage14` | Optimizer development | `selfhost/stage14/` | not started | optimization pass plan + IR strategy |
| `stage15` | Optimized toolchain rebuild | `selfhost/stage15/` | not started | stage14 completion |
| `stage16` | Final fixed-point checkpoint | `selfhost/stage16/` | not started | stage15 reproducibility harness |

## Ordered Walk

For a clean-checkout validation of active early stages:

```bash
selfhost/run-stages.sh --emu ./tools/emulator/slow32-fast
```
