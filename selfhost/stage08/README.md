# Stage 08: Subset-C Compiler (cc-min)

## What Is Here

Two things: an archiver parity gate that validates Stage 06, and
`cc-min` -- the first C compiler written in C. This is where the
bootstrap turns recursive: a C compiler compiling C.

| File/Dir | Description |
|----------|-------------|
| `cc-min.c` | Main driver |
| `cc-min-pass1.c` | Pass 1: parse subset C into a tiny IR |
| `cc-min-pass2.c` | Pass 2: IR validation and range checking |
| `cc-min-pass3.c` | Pass 3: emit SLOW-32 assembly from IR |
| `tests/` | 15 minimal test programs |
| `run-regression.sh` | Runs both archiver parity and cc-min spike |
| `run-cc-spike.sh` | Builds and end-to-end tests cc-min |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 04 compiler (`cc.fth`) to build cc-min itself
- Stage 01 assembler and Stage 03 linker (to build cc-min)
- Stage 05 assembler (to assemble cc-min's output)
- Stage 07 linker (to link cc-min's output)
- `libc/` from Stage 05

## What It Produces

`.s` assembly files from `.c` source files. The accepted subset is
growing: return values, const expressions, local variables, `if/else`,
`while`, function calls with 0-2 parameters, and conditional
expressions.

The pipeline is: cc-min compiles `.c` to `.s`, Stage 05 assembles
`.s` to `.s32o`, Stage 07 links `.s32o` to `.s32x`, emulator runs it.

## How To Test

```bash
# Full regression (archiver parity + cc-min spike)
selfhost/stage08/run-regression.sh --emu ./tools/emulator/slow32-fast

# Just the compiler spike
selfhost/stage08/run-cc-spike.sh --emu ./tools/emulator/slow32-fast
```
