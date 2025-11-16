# SLOW-32 Helper Scripts

This directory contains helper scripts for working with the SLOW-32 toolchain.

## Quick Start

For the easiest experience, source the aliases file in your shell:
```bash
source scripts/aliases.sh
```

This gives you convenient shortcuts:
- `s32run program.s32x` - Run with standard emulator
- `s32run-fast program.s32x` - Run with optimized emulator
- `s32run-qemu program.s32x` - Run with QEMU
- `s32run-trace program.s32x` - Run with instruction tracing
- `s32run-debug program.s32x` - Run with full debugging

To make this permanent, add the source command to your `~/.bashrc` or `~/.zshrc`.

## Script Overview

### `run-in-docker.sh` - Docker Emulator Wrapper

Convenient wrapper for running SLOW-32 executables in the Docker emulator container with automatic path conversion.

**Usage:**
```bash
./scripts/run-in-docker.sh [options] program.s32x [program args...]
```

**Examples:**
```bash
# Run with default emulator (slow32)
./scripts/run-in-docker.sh program.s32x

# Run with optimized C++ emulator
./scripts/run-in-docker.sh --fast program.s32x

# Run with QEMU TCG emulator
./scripts/run-in-docker.sh --qemu program.s32x

# Debug options (C++ emulators only)
./scripts/run-in-docker.sh -t program.s32x              # Trace instructions
./scripts/run-in-docker.sh -r program.s32x              # Show register changes
./scripts/run-in-docker.sh -c 1000 program.s32x         # Limit to 1000 cycles
./scripts/run-in-docker.sh -b 0x100 program.s32x        # Break at address
./scripts/run-in-docker.sh -w 0x1000-0x2000 program.s32x # Watch memory range

# Combine options
./scripts/run-in-docker.sh --fast -t -c 100 program.s32x
```

**Features:**
- Automatically converts local file paths to container paths
- Mounts project root as `/data` in container
- Supports all three emulators: slow32, slow32-fast, qemu-system-slow32
- Full access to debug options for C++ emulators

**Requirements:**
- Docker must be installed and running
- `slow32-emulator` image must be built: `docker build -t slow32-emulator -f Dockerfile.emulator .`

### `s32run` - In-Container Emulator Selector

The `s32run` script runs inside the Docker container and provides a unified interface for selecting and running different SLOW-32 emulators.

**Usage (inside container):**
```bash
s32run [options] program.s32x [args...]
```

**Emulator Selection:**
- `-f, --fast` - Use optimized C++ emulator (slow32-fast)
- `-q, --qemu` - Use QEMU TCG emulator (qemu-system-slow32)
- Default: Use standard C++ emulator (slow32)

**Debug Options (C++ emulators only):**
- `-s, --step` - Step through each instruction
- `-t, --trace` - Trace every instruction
- `-r, --registers` - Show register changes
- `-c N, --cycles N` - Limit execution to N cycles
- `-b ADDR` - Set breakpoint at address
- `-w RANGE` - Watch memory range (e.g., 0x1000-0x2000)

**Other Options:**
- `-h, --help` - Show help message

This script is automatically included in the `slow32-emulator` Docker image and is called by `run-in-docker.sh`.

### `compile.sh` - Quick Compilation Helper

Compiles C programs to SLOW-32 executables using the native LLVM backend.

**Usage:**
```bash
./scripts/compile.sh program.c [output.s32x]
```

If output filename is not specified, uses `program.s32x` by default.

### `test-quick.sh` - Quick Toolchain Test

Runs a quick test of the entire toolchain to verify everything is working correctly.

**Usage:**
```bash
./scripts/test-quick.sh
```

## Emulator Comparison

| Emulator | Speed | Features | Best For |
|----------|-------|----------|----------|
| `slow32` | ~22 MIPS | Full debug support, memory watching, breakpoints | Development, debugging |
| `slow32-fast` | ~41 MIPS | Same debug features as slow32, optimized execution | Faster testing |
| `qemu-system-slow32` | Varies | TCG translation, detailed statistics, professional emulation | Validation, performance analysis |

## Docker vs Native

**Docker advantages:**
- Clean, reproducible environment
- No need to install dependencies on host
- Easy to test against known-good baseline
- Works on any platform with Docker

**Native advantages:**
- Slightly faster execution (no container overhead)
- Easier to debug with gdb/lldb
- Direct file system access
- Better IDE integration

Use Docker for testing and validation, native for active development.
