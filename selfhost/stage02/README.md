# Stage 02: Subset-C Assembler + Archiver + Linker + Compiler

## What Is Here

The C-based tool replacements. `s32-as.c` is a SLOW-32 assembler,
`s32-ar.c` is a SLOW-32 archiver, `s32-ld.c` is a SLOW-32 linker,
and `cc-min.c` is a subset-C compiler, all written in subset C
(compiled by Stage 01's Forth compiler). They replace the Stage 01
Forth assembler, archiver, linker, and compiler in the bootstrap
pipeline. After this stage, ALL Forth tools (stage01) are no longer
needed.

| File/Dir | Description |
|----------|-------------|
| `s32-as.c` | Assembler source (subset C) |
| `s32-ar.c` | Archiver source (subset C) |
| `s32-ar-scan.c` | Archive symbol scanner source (subset C) |
| `s32-ld.c` | Linker source (subset C) |
| `cc-min.c` | Compiler main source (subset C) |
| `cc-min-pass1.c` | Compiler pass 1 (lexer/parser) |
| `cc-min-pass2.c` | Compiler pass 2 (code generation) |
| `cc-min-pass3.c` | Compiler pass 3 (optimization) |
| `crt0.s` | Stage02-specific startup object (zeros BSS, calls `__slow32_start`) |
| `mmio.s` / `mmio_no_start.s` | Minimal MMIO runtime (with/without fallback `__slow32_start`) |
| `libc/` | Minimal C library: `start.c`, `stdio.c`, `string_extra.c`, `convert.c` |
| `run-pipeline.sh` | Master A/B pipeline driver |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 01 compiler (`cc.fth`) to compile the C source (one last time)
- Stage 01 assembler (`asm.fth`) to assemble the compiler output
- Stage 01 linker (`link.fth`) to link the tools (last Forth dependency)
- Forth kernel and prelude
- Stage02 runtime sources (`crt0.s`, `mmio*.s`) provided locally, so no Stage 01 assembly files are required.

## What It Produces

- `s32-as.s32x` — assembler executable
- `s32-ar.s32x` — archiver executable
- `s32-ld.s32x` — linker executable
- `cc-min.s32x` — compiler executable (the first C-only fixed point)
- `.s32o` object files from `.s` assembly source
- `.s32a` archive files from `.s32o` object files
- `.s32x` executables from `.s32o` object files

The `libc/` directory provides the minimal C library (stdio, strings,
startup) that all C-based tools link against.

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

# Linker smoke test
selfhost/stage02/run-spike-ld.sh

# Compiler regression
selfhost/stage02/run-regression-cc-min.sh --emu ./tools/emulator/slow32-fast
```

See `run-pipeline.sh --help` for all modes.
