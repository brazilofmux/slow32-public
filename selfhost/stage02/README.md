# Stage 02: Subset-C Assembler + Archiver

## What Is Here

The first C-based tool replacements. `s32-as.c` is a SLOW-32 assembler
and `s32-ar.c` is a SLOW-32 archiver, both written in subset C (compiled
by Stage 01's Forth compiler). They replace the Stage 01 Forth assembler
and archiver in the bootstrap pipeline.

| File/Dir | Description |
|----------|-------------|
| `s32-as.c` | Assembler source (subset C) |
| `s32-ar.c` | Archiver source (subset C) |
| `s32-ar-scan.c` | Archive symbol scanner source (subset C) |
| `crt0.s` | Stage02-specific startup object (zeros BSS, calls `__slow32_start`) |
| `mmio.s` / `mmio_no_start.s` | Minimal MMIO runtime (with/without fallback `__slow32_start`) |
| `libc/` | Minimal C library: `start.c`, `stdio.c`, `string_extra.c`, `convert.c` |
| `run-pipeline.sh` | Master A/B pipeline driver (used by Stages 02-08) |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 01 compiler (`cc.fth`) to compile the C source
- Stage 01 assembler (`asm.fth`) to assemble the compiler output
- Stage 01 linker (`link.fth`) to link the result
- Forth kernel and prelude
- Stage02 runtime sources (`crt0.s`, `mmio*.s`) provided locally, so no Stage 01 assembly files are required.

## What It Produces

- `.s32o` object files from `.s` assembly source -- the same format as
  Stage 01, but built and run as a C program on the emulator.
- `.s32a` archive files from `.s32o` object files, or extracts/lists
  members from existing archives. Commands: `c`, `rc`, `t`, `x`, `d`,
  `m`, `v`, `p`, `cs`.

The `libc/` directory provides the minimal C library (stdio, strings,
startup) that all C-based tools in Stages 02-08 link against.

## How To Test

```bash
# Baseline (all-Forth) -- proves the pipeline works
selfhost/stage02/run-pipeline.sh --mode baseline --test test3

# Progressive (C assembler replaces Forth assembler)
selfhost/stage02/run-pipeline.sh --mode progressive-as --test test3

# Archive smoke tests
selfhost/stage02/run-pipeline.sh --mode stage6-ar-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-rc-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-tx-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-d-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-scan-smoke
```

See `run-pipeline.sh --help` for all modes.
