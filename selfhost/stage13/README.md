# Stage 13: cc-min-compiled Linker

## Overview

Stage 13 provides a cc-min-compiled linker (`s32-ld-port.c`), completing the
replacement of all four bootstrap tools (assembler, archiver, compiler, linker)
with cc-min-compiled C versions.

## What It Produces

- `s32-ld-port.s32x` — A full-featured linker running on the SLOW-32 emulator
- Byte-identical `.s32x` output compared to the host-native linker (parity proven)

## Features

- Merges `.text`, `.data`, `.rodata`, `.bss` sections from multiple `.s32o` files
- Iterative archive pull loop (loads members that define undefined symbols)
- All 9 relocation types: REL_32, HI20, LO12, BRANCH, JAL, CALL, PCREL_HI20, PCREL_LO12, ABS32
- Linker-defined symbols (__bss_start, __mmio_base, __heap_start, etc.)
- Dynamic expression resolution for `__mmio_base+N` patterns
- Page-aligned section layout with W^X protection
- `.s32x` executable output with 64-byte header

## Usage

```
s32-ld-port -o output.s32x [--mmio SIZE] file1.s32o file2.s32o lib.s32a ...
```

Options:
- `-o output.s32x` — Output executable path (required)
- `--mmio SIZE` — MMIO region size (e.g., `64K`, `1M`, or bytes)
- Input files are auto-detected as `.s32o` (object) or `.s32a` (archive)

## Build & Test

```bash
bash selfhost/stage13/run-spike.sh
```

This bootstraps through stages 1-12, compiles the linker with cc-min,
and runs two verification tests:

1. **Smoke test** — Linked executable returns rc=42
2. **Archive test** — Links against `.s32a` archive, runs correctly

## Files

- `s32ld_min.h` — cc-min compatible header (constants, prototypes)
- `s32-ld-port.c` — Linker implementation (~1050 lines)
- `run-spike.sh` — Build and test script

## Dependencies

- Stage 12 (provides cc-min compiler, assembler, archiver, runtime)
- Stage 07 C linker (from stage08 bootstrap, used to link the cc-min linker itself)
