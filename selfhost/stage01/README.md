# Stage 01: Forth Assembler

## What Is Here

A two-pass assembler written in Forth that reads SLOW-32 assembly
source (`.s`) and produces object files (`.s32o`) with relocations
and symbol tables.

| File | Description |
|------|-------------|
| `asm.fth` | The assembler (~1200 lines of Forth) |
| `crt0_minimal.s` | Minimal C runtime startup code |
| `mmio_minimal.s` | Minimal MMIO runtime (putchar, open, read, write, memset, memcpy) |
| `test*.s` | Assembly test sources |
| `run-regression.sh` | Regression test runner |
| `run-test*.fth` | Forth scripts that drive assembly of individual tests |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Forth kernel (`forth/kernel.s32x`) and prelude (`forth/prelude.fth`)

## What It Produces

`.s32o` object files -- the linkable format with relocations. Also
provides `crt0_minimal.s` and `mmio_minimal.s`, the bootstrap runtime
sources used by all later stages.

## How To Test

```bash
selfhost/stage01/run-regression.sh test1
selfhost/stage01/run-regression.sh test3
```
