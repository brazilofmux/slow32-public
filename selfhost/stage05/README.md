# Stage 05: Subset-C Assembler

## What Is Here

The first C-based tool replacement. `s32-as.c` is a SLOW-32 assembler
written in subset C (compiled by Stage 04's Forth compiler). It
replaces the Stage 01 Forth assembler in the bootstrap pipeline.

| File/Dir | Description |
|----------|-------------|
| `s32-as.c` | Assembler source (subset C) |
| `crt0.s` | Stage05-specific startup object (zeros BSS, calls `__slow32_start`) |
| `mmio.s` / `mmio_no_start.s` | Minimal MMIO runtime (with/without fallback `__slow32_start`) |
| `libc/` | Minimal C library: `start.c`, `stdio.c`, `string_extra.c`, `convert.c` |
| `run-pipeline.sh` | Master A/B pipeline driver (used by Stages 05-08) |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 04 compiler (`cc.fth`) to compile the C source
- Stage 01 assembler (`asm.fth`) to assemble the compiler output
- Stage 03 linker (`link.fth`) to link the result
- Forth kernel and prelude
- Stage05 runtime sources (`crt0.s`, `mmio*.s`) provided locally, so no Stage 01 assembly files are required.

## What It Produces

`.s32o` object files from `.s` assembly source -- the same format as
Stage 01, but built and run as a C program on the emulator.

The `libc/` directory provides the minimal C library (stdio, strings,
startup) that all C-based tools in Stages 05-08 link against.

## How To Test

```bash
# Baseline (all-Forth) -- proves the pipeline works
selfhost/stage05/run-pipeline.sh --mode baseline --test test3

# Progressive (C assembler replaces Forth assembler)
selfhost/stage05/run-pipeline.sh --mode progressive-as --test test3
```

See `run-pipeline.sh --help` for all modes including Stage 06 archive
smoke tests.
