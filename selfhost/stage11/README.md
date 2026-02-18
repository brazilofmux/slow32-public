# Stage 11: cc-min-compiled Assembler

## What This Does

Compiles the standard assembler (`stage05/s32-as.c`) with cc-min to produce
a cc-min-compiled assembler (`s32-as-port.s32x`). This exercises cc-min on
a real-world ~800-line program and begins replacing Forth-compiled tools
with cc-min-compiled ones.

## How It Works

1. Bootstrap via stage09 (obtains Gen2 cc-min, stage05 assembler, runtime)
2. cc-min compiles `s32-as-port.c` (mechanical port of `s32-as.c`)
3. Stage05 assembler assembles the output
4. Forth linker links with libc to produce `s32-as-port.s32x`
5. Parity test: both assemblers assemble the same inputs, compare `.s32o`
6. Smoke test: cc-min-compiled assembler assembles a test, link + run

## Porting Changes (s32-as.c → s32-as-port.c)

- `uint8_t`/`uint16_t`/`uint32_t`/`int32_t`/`long` → `int` or `char`
- `sizeof(struct)` → numeric constants (40, 32, 16, 16)
- `FILE *` → `int` (selfhost libc convention)
- Block-scoped declarations hoisted to function tops
- `rd32()` sign-extension fix: `& 255` masks on byte reads
- `const` removed from local variables
- Ternary `?:` replaced with if/else
- `enum` replaced with `#define`
- `u` suffixes removed from integer literals
- Postfix `++`/`--` replaced with `x = x + 1` / `x = x - 1`
- Bitwise NOT `~` replaced with constant equivalents (`~0x7F` → `-128`, `~3` → `-4`)
- `for(;;)` replaced with `while(1)`
- `static` removed from function definitions

## Running

```bash
bash selfhost/stage11/run-spike.sh
bash selfhost/stage11/run-spike.sh --keep-artifacts
bash selfhost/stage11/run-spike.sh --emu /path/to/emulator
```

## Status

Complete. Parity test (byte-identical .s32o on two inputs) and smoke test (rc=42) pass.
