# Stage 00: Minimal Bootstrap Emulator

## What Is Here

A self-contained SLOW-32 CPU emulator written in portable C11/POSIX.
This is the **trust root** of the entire bootstrap chain -- everything
else runs on top of it.

| File | Description |
|------|-------------|
| `s32-emu.c` | Emulator source (~735 lines of C) |
| `Makefile` | Builds the emulator with the host C compiler |

## What It Needs

- A host C compiler (`cc`, `gcc`, or `clang`)
- Standard C library and POSIX headers
- Nothing from the SLOW-32 toolchain

## What It Produces

A host-native binary (`s32-emu`) that can load and execute `.s32x`
SLOW-32 executables. Supports all integer instructions and MMIO
ring-buffer I/O.

## How To Build and Test

```bash
make -C selfhost/stage00
make -C selfhost/stage00 test
```

The smoke test boots the Forth kernel with `1 2 + . CR BYE` and
expects `3`.
