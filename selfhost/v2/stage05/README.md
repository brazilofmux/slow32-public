# Stage05 Progressive Pipeline Harness

This stage provides an A/B pipeline driver for replacing Forth tools with subset-C tools.

Script:
- `run-pipeline.sh`

Modes:
- `baseline`: Stage4 `cc.fth` + Stage1 Forth assembler + Stage3 Forth linker
- `progressive-as`: Stage4 `cc.fth` + Stage5 `s32-as.c` (C assembler) + Stage3 linker
- `progressive-as-ar`: same as `progressive-as` plus Stage6 `s32-ar.c` archive smoke-check

Examples:
- `selfhost/v2/stage05/run-pipeline.sh --mode baseline --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as --test test3`
- `selfhost/v2/stage05/run-pipeline.sh --mode progressive-as-ar --test test3`
