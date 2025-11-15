# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a fork of QEMU integrating the SLOW-32 CPU architecture. QEMU is a machine emulator and virtualizer. This repository contains:
- Upstream QEMU codebase (main emulation framework)
- SLOW-32 custom toolchain in `slow-32/` directory
- SLOW-32 TCG (Tiny Code Generator) backend to run SLOW-32 binaries in QEMU

The goal is to enable `qemu-system-slow32 -kernel program.s32x` to execute SLOW-32 executables.

## Build System

QEMU uses Meson/Ninja for building. Configuration happens once, then incremental builds are fast.

### Initial Configuration

```bash
mkdir build
cd build
../configure --target-list=slow32-softmmu
```

To configure with additional targets (if needed):
```bash
../configure --target-list=x86_64-softmmu,slow32-softmmu
```

### Building

```bash
# From the build directory
ninja

# Or from repository root (will auto-detect build directory)
make
```

### Cleaning

```bash
# Clean build artifacts
make clean

# Full clean (removes configuration)
make distclean
```

## SLOW-32 Architecture

### ISA Summary
- 32-bit RISC architecture with 32 registers (r0 hardwired to zero)
- Fixed 32-bit instruction encoding
- No condition codes (comparisons produce 0/1 in registers)
- Register conventions: r1-r2=return, r3-r10=args, r29=sp, r30=fp, r31=lr
- Special instructions: `debug` (character output), `yield`, `halt`

### Memory Layout
- Code segment: 0x0000_0000 - 0x000F_FFFF (1MB, execute-only, W^X enforced)
- Data segment: 0x0010_0000 - 0x0FFF_FFFF (255MB, read/write)
- Stack: starts at 0x0FFF_FFF0, grows downward
- Optional MMIO window (linker provides `__mmio_base` symbol)

### Toolchain Location
The complete SLOW-32 toolchain lives in `slow-32/`:
- Assembler: `slow-32/tools/assembler/slow32asm`
- Linker: `slow-32/tools/linker/s32-ld`
- Emulators: `slow-32/tools/emulator/slow32` (reference) and `slow32-fast` (~385 MIPS)
- Runtime: `slow-32/runtime/` (crt0, libc, intrinsics)
- LLVM backend: Built in `~/llvm-project/build/bin` (Clang with `-target slow32-unknown-none`)

See `slow-32/CLAUDE.md` and `slow-32/README.md` for toolchain commands.

## SLOW-32 TCG Integration

### Current Status (see `docs/slow32-tcg/todo.md`)
- ✅ Target skeleton created (`target/slow32/`)
- ✅ Machine implementation (`hw/slow32/slow32-tcg.c`)
- ✅ Basic TCG translation for arithmetic, loads/stores, branches
- ⚠️ `.s32x` loader not yet implemented (currently loads raw binaries)
- ⚠️ ISA not complete (missing some branch/comparison variants)

### Key Files
- `target/slow32/` - CPU state definition and TCG translation
  - `cpu.h`, `cpu.c` - Slow32CPU state and QOM class
  - `translate.c` - Instruction decoder and TCG emission
  - `helper.h`, `helper.c` - Runtime helpers (debug, halt, complex ops)
- `hw/slow32/` - Machine and device emulation
  - `slow32-tcg.c` - Simple machine for bare-metal execution
- `configs/targets/slow32-softmmu.mak` - Build configuration
- `docs/slow32-tcg/` - Design documentation
  - `overview.md` - Architecture requirements
  - `todo.md` - Implementation checklist

### Reference Implementation
The authoritative behavior is defined by `slow-32/tools/emulator/slow32.c`. When implementing TCG translation:
1. Decode logic matches `slow32.c` opcode tables
2. Instruction semantics must match reference emulator
3. Memory layout follows `slow-32/tools/emulator/s32x_loader.h`
4. Test outputs should match `slow32 -t` trace format

### Building QEMU with SLOW-32

```bash
# Configure (only needed once or after distclean)
./configure --target-list=slow32-softmmu

# Build
ninja -C build

# The binary will be at:
# build/qemu-system-slow32
```

### Testing TCG Backend

```bash
# Build a SLOW-32 binary (using the toolchain)
cd slow-32
./tools/compile-c.sh examples/hello.c hello.s32x

# Run in reference emulator
./tools/emulator/slow32 hello.s32x

# Run in QEMU (once loader is implemented)
cd ..
./build/qemu-system-slow32 -M slow32 -kernel slow-32/hello.s32x -nographic
```

Currently QEMU expects raw binaries. The `.s32x` loader implementation is pending in Phase 3 of the TODO.

## Testing

### QEMU Tests

```bash
# Run all tests
make check

# Run specific test suites
make check-qtest        # Device tests
make check-tcg          # TCG translator tests
make check-unit         # Unit tests
```

### SLOW-32 Toolchain Tests

```bash
cd slow-32

# Quick smoke test
./scripts/test-quick.sh

# Full regression suite
cd regression
./run-tests.sh

# Test specific case
./run-tests.sh feature-arithmetic
```

### Comparing QEMU vs Reference Emulator

```bash
# Run same binary in both and compare traces
slow-32/tools/emulator/slow32 -t program.s32x > ref_trace.txt
./build/qemu-system-slow32 -M slow32 -kernel program.s32x -d in_asm,exec > qemu_trace.txt
diff ref_trace.txt qemu_trace.txt
```

## Code Architecture

### QEMU Target Backend Structure
Each CPU architecture in QEMU follows this pattern:
- `target/<arch>/cpu.h` - CPU state (registers, flags, MMU state)
- `target/<arch>/cpu.c` - QOM class registration, reset, initialization
- `target/<arch>/translate.c` - Decode guest instructions → emit TCG ops
- `target/<arch>/helper.c` - Runtime helper functions for complex operations
- `hw/<arch>/` - Machine definitions, device models, memory maps

### TCG Translation Pipeline
1. Fetch guest instruction at PC
2. Decode instruction fields (opcode, registers, immediate)
3. Emit TCG intermediate ops (tcg_gen_* functions)
4. TCG backend compiles to host code (x86_64, ARM, etc.)
5. Execute translated block, exit on branches/exceptions

SLOW-32 translation happens in `target/slow32/translate.c` using `DisasContext` to track state during translation.

### Loader and Executable Format
SLOW-32 uses custom binary formats:
- `.s32o` - Object files with relocations (defined in `slow-32/tools/emulator/slow32_format.h`)
- `.s32x` - Linked executables with headers specifying code/data sizes

The loader must parse `.s32x` headers to set up:
- Execute-only code region
- Read/write data region
- Stack pointer (r29) initialization
- Entry point (PC)
- MMIO base register (if used)

Reference: `slow-32/tools/emulator/s32x_loader.h`

## Common Development Workflows

### Adding New TCG Instructions

1. Check opcode definition in `slow-32/docs/INSTRUCTION-SET.md`
2. Find reference implementation in `slow-32/tools/emulator/slow32.c`
3. Add opcode enum constant to `target/slow32/translate.c`
4. Implement decoder case in `slow32_tr_translate_insn()`
5. Emit TCG ops (e.g., `tcg_gen_add_i32()` for arithmetic)
6. Test with minimal assembly program

### Adding Helper Functions

For operations too complex to inline (e.g., MUL/DIV, MMIO):

1. Declare in `target/slow32/helper.h`: `DEF_HELPER_*()` macro
2. Implement in `target/slow32/helper.c`
3. Call from `translate.c`: `gen_helper_*()`
4. Helper has access to full CPU state (`CPUSlow32State *env`)

### Implementing the .s32x Loader

The loader needs to:
1. Read `.s32x` header (magic bytes, section sizes, entry point)
2. Allocate QEMU MemoryRegions for code/data/stack
3. Mark code region execute-only (enforce W^X)
4. Initialize CPU registers (PC, SP, MMIO base)
5. Hook into `-kernel` option in `hw/slow32/slow32-tcg.c`

See `slow-32/tools/emulator/s32x_loader.h` for format details.

## Important Notes

### SLOW-32 Binary Linking
- **Always link with `crt0.s32o` first** - Contains `_start` entry point at address 0
- Link order: `crt0.s32o` → program objects → `libs32.s32a` → `libc.s32a`
- Use `slow-32/tools/compile-c.sh` helper script for correct linking

### Memory Protection
- SLOW-32 enforces W^X (write XOR execute)
- Code segment is execute-only at hardware level
- Attempts to write code or execute data must fault
- QEMU should mirror this with MemoryRegion permissions

### MMIO and Debug Output
- The `DEBUG` instruction outputs a character (only I/O in minimal ISA)
- MMIO support is optional (enabled via linker `--mmio` flag)
- For initial testing, `DEBUG` can write directly to stdout
- Later, integrate with QEMU chardev infrastructure (`qemu_chr_fe_write_all`)

### ISA Documentation
- `slow-32/docs/INSTRUCTION-SET.md` - Complete instruction reference
- `slow-32/docs/file-formats.md` - Object and executable format specs
- `slow-32/tools/emulator/slow32.c` - Executable specification (shows exact semantics)

### Performance Notes
- Reference interpreter: ~45 MIPS (`slow32`)
- Optimized interpreter: ~385 MIPS (`slow32-fast`)
- TCG backend should eventually exceed interpreter performance through JIT compilation
- Instruction timing: most ops 1 cycle, memory 3 cycles, MUL 32 cycles, DIV 64 cycles

## External Dependencies

- LLVM 22+ with SLOW-32 backend (built in `~/llvm-project`)
- Clang/LLC in `~/llvm-project/build/bin`
- GCC or Clang for building QEMU itself
- Meson ≥ 1.5.0 and Ninja for build system
- Python 3 for build scripts

## Additional Resources

- `AGENTS.md` - High-level integration guide for SLOW-32 + QEMU
- `slow-32/AGENTS.md` - Detailed toolchain development guide
- `slow-32/CLAUDE.md` - Quick reference for SLOW-32 commands
- QEMU upstream docs: https://www.qemu.org/documentation/
- QEMU developer guide: https://www.qemu.org/docs/master/devel/
- TCG internals: `docs/devel/tcg.rst`, `tcg/README`
