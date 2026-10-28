# SLOW-32 Helper Scripts

This directory contains helper scripts for working with the SLOW-32 toolchain.

## Quick Start

For the easiest experience, source the aliases file in your shell (they wrap `run-in-docker.sh`):
```bash
source scripts/aliases.sh
```

This gives you convenient shortcuts:

- `s32run program.s32x` - Run with the default engine (slow32-dbt)
- `s32run-fast program.s32x` - Run with optimized emulator
- `s32run-qemu program.s32x` - Run with QEMU
- `s32run-trace program.s32x` - `-t` (instruction tracing; add `-i` for the interpreter)
- `s32run-debug program.s32x` - `-t -r` (likewise)

To make this permanent, add the source command to your `~/.bashrc` or `~/.zshrc`.

## Script Overview

### `run-in-docker.sh` - Container Emulator Wrapper

Runs SLOW-32 executables in the `slow32:emulator` container through
`s32run`, converting local `.s32x` paths to container paths.  Uses podman
when it is installed, docker otherwise.

**Usage:**
```bash
./scripts/run-in-docker.sh [s32run options] program.s32x [program args...]
```

**Examples:**
```bash
./scripts/run-in-docker.sh program.s32x            # slow32-dbt (the default)
./scripts/run-in-docker.sh --fast program.s32x     # slow32-fast
./scripts/run-in-docker.sh --interp program.s32x   # slow32, the reference interpreter
./scripts/run-in-docker.sh --qemu program.s32x     # qemu-system-slow32 (TCG)

# Debug options belong to the reference interpreter
./scripts/run-in-docker.sh -i -t program.s32x              # Trace instructions
./scripts/run-in-docker.sh -i -r program.s32x              # Show register changes
./scripts/run-in-docker.sh -i -c 1000 program.s32x         # Limit to 1000 cycles
./scripts/run-in-docker.sh -i -b 0x100 program.s32x        # Break at address
./scripts/run-in-docker.sh -i -w 0x1000-0x2000 program.s32x # Watch memory range
```

**Features:**

- Mounts the project root as `/data` in the container
- Selects any of the four engines in the image: slow32-dbt, slow32, slow32-fast, qemu-system-slow32

**Requirements:**

- podman or docker
- the `slow32:emulator` image: `podman build -t slow32:emulator -f Dockerfile.emulator .`
  (an older local image named `slow32-emulator` is used if that is all there is)

### `s32run` - In-Container Emulator Selector

`s32run` runs inside the container images and picks the engine.

**Usage (inside container):**
```bash
s32run [options] program.s32x [args...]
```

**Emulator Selection:**

- `-d, --dbt` - Dynamic binary translator (slow32-dbt); the default
- `-i, --interp` - Reference interpreter (slow32)
- `-f, --fast` - Pre-decoded interpreter (slow32-fast)
- `-q, --qemu` - QEMU TCG (qemu-system-slow32); arguments go through `-append`

**Debug Options:**

- `-s, --step` - Step through each instruction
- `-t, --trace` - Trace every instruction
- `-r, --registers` - Show register changes
- `-c N, --cycles N` - Limit execution to N cycles
- `-b ADDR` - Set breakpoint at address
- `-w RANGE` - Watch memory range (e.g., 0x1000-0x2000)

These are the reference interpreter's flags; use them with `-i`.  `s32run`
drops them for slow32-fast and qemu and passes them through to slow32-dbt,
whose own `-s`/`-t` mean other things (`slow32-dbt -h`).

**Other Options:**

- `-h, --help` - Show help message

### `compile.sh` - Quick Compilation Helper

Compiles C programs to SLOW-32 executables using the native LLVM backend.

**Usage:**
```bash
./scripts/compile.sh program.c [output.s32x]
```

If output filename is not specified, uses `program.s32x` by default.
A `.cpp` input is compiled with clang++ (`-fno-exceptions -fno-rtti`; for
full C++ use `s32c++` in the toolchain image).  `--mmio SIZE` as the third
argument links with MMIO and `libc_mmio.s32a` instead of `libc_debug.s32a`:
`./scripts/compile.sh prog.c prog.s32x --mmio 64K`.

### `test-quick.sh` - Quick Toolchain Test

Runs a quick test of the entire toolchain to verify everything is working correctly.

**Usage:**
```bash
./scripts/test-quick.sh
```

### Other scripts

- `build-kit-image.sh` - builds the `slow32:kit` image from a machine that has the kit (`~/s32x`); `--push` builds both architectures
- `build-libcxx.sh` - builds libc++/libc++abi for SLOW-32 from the llvm-project tree
- `build-libutf.sh [--check]` - builds libutf (`~/utf`) with stage08 cc and runs its stress harness
- `test-stage08-graphics.sh`, `test-stage08-doom.sh` - stage08-built tube demos, ppu reel and DOOM must reproduce the clang builds' frame hashes
- `slow32-toolchain.cmake` - CMake toolchain file (clang, slow32asm, s32-ar; static libraries only)

## Emulator Comparison

See [`docs/EMULATORS.md`](../docs/EMULATORS.md) for the five engines and
their measured speeds.

## Container vs Native

**Container advantages:**

- Clean, reproducible environment
- No need to install dependencies on host
- Easy to test against known-good baseline
- Works on any platform with podman or docker

**Native advantages:**

- Slightly faster execution (no container overhead)
- Easier to debug with gdb/lldb
- Direct file system access
- Better IDE integration

Use the containers for testing and validation, native for active development.
