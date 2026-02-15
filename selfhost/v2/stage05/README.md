# Stage05 Progressive Pipeline Harness

This stage provides an A/B pipeline driver for replacing Forth tools with subset-C tools.

Script:
- `run-pipeline.sh`

Modes:
- `baseline`: Stage4 `cc.fth` + Stage1 Forth assembler + Stage3 Forth linker
- `progressive-as`: Stage4 `cc.fth` + Stage5 `s32-as.c` (C assembler) + Stage3 linker
- `progressive-as-ar`: same as `progressive-as` plus Stage6 `s32-ar.c` archive smoke-check
- `progressive-as-ar-scan`: same as `progressive-as-ar` but runs Stage6 with opt-in `s` archive scan flag (`cmd=cs`)
- `stage6-ar-smoke`: focused Stage6 smoke using stable Stage1 assembler for Stage6 binary build
- `stage6-ar-rc-smoke`: focused Stage6 replace-on-existing smoke (`cmd=rc`) with extract+byte-compare verification
- `stage6-ar-tx-smoke`: focused Stage6 list/extract smoke (`cmd=t/x`) with extract byte-compare verification
- `stage6-ar-scan-smoke`: focused Stage6 scan smoke using `s32-ar-scan.c` and `cmd=cs`

Examples:
- `selfhost/v2/stage05/run-pipeline.sh --mode baseline --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as-ar --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as-ar-scan --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode stage6-ar-smoke`
- `selfhost/v2/stage05/run-pipeline.sh --mode stage6-ar-rc-smoke`
- `selfhost/v2/stage05/run-pipeline.sh --mode stage6-ar-tx-smoke`
- `selfhost/v2/stage05/run-pipeline.sh --mode stage6-ar-scan-smoke`
