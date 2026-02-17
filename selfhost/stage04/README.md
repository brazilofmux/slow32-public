# Stage 04: Forth-Hosted C Compiler

## What Is Here

A C compiler written in Forth that reads subset-C source files and
emits SLOW-32 assembly. This is the bridge from Forth to C -- once
this stage works, all subsequent tools can be written in C.

| File/Dir | Description |
|----------|-------------|
| `cc.fth` | The compiler (~145KB of Forth) |
| `include/` | Subset-C standard library headers |
| `tests/` | ~86 test programs (unit + idiom coverage) |
| `validation/` | Real-world C programs (assembler, archiver, disassembler, dumper) |
| `SUBSET-C.md` | Subset-C language contract |
| `run-regression.sh` | Staged regression (A/B/C/D) |
| `run-subset-conformance.sh` | Subset-C conformance checker |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Forth kernel (`forth/kernel.s32x`) and prelude (`forth/prelude.fth`)
- Stage 01 assembler and Stage 03 linker (for compile-assemble-link-run tests)
- `crt0_minimal.s` and `mmio_minimal.s` from Stage 01

## What It Produces

`.s` assembly files from `.c` source files. The supported subset
includes `int`, `char`, pointers, arrays, structs, enums, `typedef`,
control flow (`if/else/while/for/switch`), function calls, string
literals, preprocessor macros, and long-branch materialization.

The `validation/` directory holds utility programs written in subset C
that become the source for Stages 05-07.

## How To Test

```bash
selfhost/stage04/run-regression.sh
```

This runs tests in stages: A (basic), B (intermediate), C (advanced),
and optionally D (validation programs like `slow32dump.c`).
