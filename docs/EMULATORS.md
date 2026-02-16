# SLOW-32 Emulators

Five emulators exist with different trade-offs between capability, speed, and portability.

## Summary

| Emulator | Type | Speed | Full ISA | Full MMIO | Intrinsic Hooks | Source |
|----------|------|-------|----------|-----------|-----------------|--------|
| s32-emu | Interpreter | ~50 MIPS | Subset | Minimal | No | `selfhost/stage00/` |
| slow32 | Interpreter | ~240 MIPS | Yes | Yes | No | `tools/emulator/` |
| slow32-fast | Pre-decoded | ~240 MIPS | Yes | Yes | No | `tools/emulator/` |
| qemu-system-slow32 | TCG JIT | ~1 BIPS | Yes | Yes | Yes | `~/qemu` (separate repo) |
| slow32-dbt | DBT JIT | ~6 BIPS | Yes | Yes | Yes | `tools/dbt/` |

## s32-emu (Selfhost Bootstrap Emulator)

A minimal interpretive emulator used in the selfhost bootstrap chain. Implements
enough of the ISA to run the Forth kernel and selfhost toolchain. Supports a
restricted MMIO model (ring buffer I/O with a fixed set of opcodes).

- **Build**: `make -C selfhost/stage00` (gcc, no dependencies)
- **Use case**: Selfhost bootstrap, CI validation
- **Limitations**: Subset of instructions, limited MMIO

## slow32 (Reference Interpreter)

The reference emulator. Classic switch-dispatch interpreter with full instruction
set and complete MMIO ring buffer support. Supports debug flags for tracing,
stepping, breakpoints, and memory watches.

- **Build**: `make emulator` (gcc, no dependencies)
- **Run**: `./tools/emulator/slow32 program.s32x`
- **Debug flags**: `-s` step, `-t` trace, `-r` register changes, `-b ADDR` breakpoint, `-w RANGE` memory watch, `-c N` cycle limit
- **Performance**: ~240 MIPS at -O2

## slow32-fast (Pre-decoded Interpreter)

Pre-decodes all instructions at load time into a decoded struct, then runs a
function-pointer dispatch loop. Eliminates shift/mask operations from the hot
path and pre-computes branch targets.

- **Build**: Built alongside slow32 by `make emulator`
- **Run**: `./tools/emulator/slow32-fast program.s32x`
- **Performance**: ~240 MIPS (similar to slow32 -O2; the gcc optimizer closes
  most of the gap that pre-decoding would otherwise provide)

## qemu-system-slow32 (QEMU TCG)

A full QEMU system emulator using the Tiny Code Generator (TCG) backend.
Translates SLOW-32 basic blocks to host code on the fly. Hooks intrinsic
functions (strlen, memcpy, strcpy, memswap, sin, cos, tan, etc.) for
native-speed execution of common library calls.

- **Build**: Separate `~/qemu` checkout with SLOW-32 target patches
- **Availability**: `slow32:emulator` Docker container, or build from source
- **Run**: `qemu-system-slow32 -kernel program.s32x`
- **Performance**: ~1 BIPS

## slow32-dbt (Dynamic Binary Translator)

The fastest emulator. A custom dynamic binary translator that JIT-compiles
SLOW-32 code to x86-64 (or AArch64) native code. Features include:

- **Superblock compilation**: Traces across basic block boundaries
- **Register allocation**: Maps guest registers to host registers
- **Constant propagation / folding**: Eliminates redundant computation
- **Bounds check elimination**: Removes redundant memory access checks
- **Direct block chaining**: Patches jumps between translated blocks
- **Intrinsic hooking**: Recognizes memcpy, memset, memmove, strlen, memswap
  plus math functions (via symbol table lookup) and replaces them with native
  host calls

Architecture support:

| Host Arch | Status | Notes |
|-----------|--------|-------|
| x86-64 | Solid | Full feature set, well-tested |
| AArch64 | Initial | Works, but superblocks have a known bug |

- **Build**: `make dbt` (gcc, auto-detects host architecture)
- **Run**: `./tools/dbt/slow32-dbt program.s32x`
- **Performance**: ~6 BIPS, approximately 68% of native performance

See `docs/dbt/` for design documents and stage specifications.

## Docker Containers

Two containers provide clean environments without requiring local builds:

- **slow32:toolchain** — Full development environment with LLVM, clang, assembler, linker, and all emulators
- **slow32:emulator** — Lightweight runtime with slow32, slow32-fast, slow32-dbt, and qemu-system-slow32

```bash
# Run a program with the emulator container
./scripts/run-in-docker.sh program.s32x              # Default emulator
./scripts/run-in-docker.sh --fast program.s32x        # slow32-fast
./scripts/run-in-docker.sh --qemu program.s32x        # QEMU TCG
```
