# Stage 03: Forth Linker

## What Is Here

A linker written in Forth that merges `.s32o` object files, resolves
symbols from `.s32a` archives, and produces `.s32x` executables.
Together with Stages 01 and 02, this completes the Forth-based
toolchain.

| File | Description |
|------|-------------|
| `link.fth` | The linker (~1100 lines of Forth) |
| `ar.fth` | Symlink to Stage 02 archiver |
| `run-regression.sh` | Regression runner (test3, kernel, archive targets) |
| `run-selfhost-kernel.sh` | Full self-hosting kernel pipeline |
| `run-*.fth` | Forth scripts for manual test runs |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Forth kernel (`forth/kernel.s32x`) and prelude (`forth/prelude.fth`)
- Stage 01 assembler and Stage 02 archiver (for test targets)

## What It Produces

`.s32x` executable files with resolved symbols, merged sections
(`.text/.data/.rodata/.bss`), W^X flags, and MMIO configuration.

The self-hosting kernel gate (`run-selfhost-kernel.sh`) assembles
`crt0_minimal.s`, `mmio_minimal.s`, and `forth/kernel.s` from source,
links them into `kernel-selfhost.s32x`, and boots it as a smoke test.

## How To Test

```bash
selfhost/stage03/run-regression.sh test3
selfhost/stage03/run-regression.sh kernel
selfhost/stage03/run-regression.sh archive
```
