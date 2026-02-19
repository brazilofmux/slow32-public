# Stage 07: Subset-C Linker

## What Is Here

A C-based replacement for the Stage 03 Forth linker. Links `.s32o`
object files and resolves symbols from `.s32a` archives to produce
`.s32x` executables.

| File/Dir | Description |
|----------|-------------|
| `s32-ld.c` | Linker source (subset C) |
| `run-spike.sh` | Spike test runner |
| `run-archive-spike.sh` | Archive-resolution test scaffold |
| `run-reloc-bisect.sh` | Relocation codegen bisect helper |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Stage 04 compiler, Stage 01 assembler, Stage 03 linker (to build)
- `libc/` from Stage 02
- `crt0_minimal.s` and `mmio_minimal.s` from Stage 01

## What It Produces

`.s32x` executable files from `.s32o` objects and `.s32a` archives.
Supports section merging (`.text/.data/.rodata/.bss`), relocations
(`REL_32`, `REL_HI20`, `REL_LO12`, `REL_BRANCH`, `REL_JAL`), and
symbol-aware archive member resolution.

## How To Test

```bash
# Basic linker spike
selfhost/stage07/run-spike.sh --emu ./tools/emulator/slow32-fast

# With relocation spike
selfhost/stage07/run-spike.sh --emu ./tools/emulator/slow32-fast --with-reloc-spike

# Archive resolution
selfhost/stage07/run-archive-spike.sh --emu ./tools/emulator/slow32-fast
```
