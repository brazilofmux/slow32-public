# Stage 12: cc-min-compiled Archiver

## What This Does

Compiles the archiver (`stage06/s32-ar.c`) with cc-min to produce a
cc-min-compiled archiver (`s32-ar-port.s32x`). This archiver creates `.s32a`
archives with a symbol index, so they work correctly with the Forth linker.

## How It Works

1. Bootstrap via stage11 (obtains Gen2 cc-min, stage05 assembler, runtime)
2. cc-min compiles `s32-ar-port.c` (mechanical port of `s32-ar.c` + symbol index)
3. Stage05 assembler assembles the output
4. Forth linker links with libc to produce `s32-ar-port.s32x`
5. Functional test: create archive from libc .s32o files, verify member count
6. Link test: link a test program against cc-min-created archive, run (rc=42)

## Key Addition: Symbol Index

The original `stage06/s32-ar.c` creates archives with `nsymbols=0`. The Forth
linker (`stage03/link.fth`) uses the symbol index to decide which archive
members to pull in. Without it, nothing gets linked from archives.

`s32-ar-port.c` adds `build_symbol_index()` which scans `.s32o` member data
for global defined symbols and records them in the archive's symbol index.
Each entry is 8 bytes: name offset into the archive string table + member index.

## Porting Changes (s32-ar.c -> s32-ar-port.c)

- `uint8_t`/`uint32_t`/`int32_t`/`size_t`/`long` -> `int` or `char`
- `sizeof(struct)` -> computed by cc-min (structs retained)
- `FILE *` -> `int` (selfhost libc convention)
- `stdout`/`stderr` -> globals resolved by linker against libc
- Block-scoped declarations hoisted to function scope
- `const` removed from local variables
- `static` removed from function definitions
- Postfix `++`/`--` replaced with `x = x + 1`
- `for(;;)` replaced with `while(1)`
- `(void)x` casts removed
- `putchar(ch)` replaced with `fputc(ch, stdout)`
- Backslash char literal `'\\'` replaced with numeric `92`

## Running

```bash
bash selfhost/stage12/run-spike.sh
bash selfhost/stage12/run-spike.sh --keep-artifacts
bash selfhost/stage12/run-spike.sh --emu /path/to/emulator
```

## Status

Complete. Functional test (create+list archive) and link test (Forth linker resolves
symbols from cc-min-created archive, runs program with rc=42) both pass.
