# Stage 06: Subset-C Archiver

## What Is Here

A C-based replacement for the Stage 02 Forth archiver. Supports
the full archive command surface: create, replace, list, extract,
delete, move, verbose list, print member, and symbol scanning.

| File | Description |
|------|-------------|
| `s32-ar.c` | Archiver source (subset C) |
| `s32-ar-scan.c` | Archive symbol scanner source (subset C) |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 04 compiler, Stage 01 assembler, Stage 03 linker (to build)
- Stage 02 pipeline harness (`run-pipeline.sh`)
- `libc/` from Stage 02

## What It Produces

`.s32a` archive files from `.s32o` object files, or extracts/lists
members from existing archives. Commands: `c`, `rc`, `t`, `x`, `d`,
`m`, `v`, `p`, `cs`.

## How To Test

All testing is driven through the Stage 02 pipeline:

```bash
selfhost/stage02/run-pipeline.sh --mode stage6-ar-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-rc-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-tx-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-d-smoke
selfhost/stage02/run-pipeline.sh --mode stage6-ar-scan-smoke
```
