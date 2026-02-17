# Stage 02: Forth Archiver

## What Is Here

An archiver written in Forth that creates and manipulates `.s32a`
archive files (the SLOW-32 equivalent of Unix `.a` static libraries).

| File | Description |
|------|-------------|
| `ar.fth` | The archiver (~600 lines of Forth) |
| `run-regression.sh` | Regression test runner |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Forth kernel (`forth/kernel.s32x`) and prelude (`forth/prelude.fth`)
- Stage 01 assembler (`asm.fth`) for regression tests

## What It Produces

`.s32a` archive files from `.s32o` object files. Supports create,
list, extract, replace, delete, move, verbose list, and print member.

## How To Test

```bash
selfhost/stage02/run-regression.sh test3
```

The test assembles `test3.s`, archives it, lists the archive, extracts
the member, and byte-compares with the original.
