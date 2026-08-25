# SLOW-32 Project

A deliberately inefficient 32-bit RISC CPU architecture with complete toolchain.

- **Purpose**: Educational CPU design and sandboxed compute engine
- **Status**: Active. Two independent C compilers (an LLVM backend and a
  self-hosted one that share an ABI), five execution engines, a graphics
  stack, and a broad application corpus — see [Current Status](#current-status)

## Quick Start

```bash
# Build everything
make

# Compile C to executable (Native LLVM target)
# Note: All optimization levels (-O0, -O1, -O2) are supported
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x

# Or assemble and run assembly directly
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x
```

## Binary Artifact Policy

Some SLOW-32 binaries are intentionally checked into git because they are
bootstrap assets and host-agnostic across little-endian machines.

- Tracked bootstrap artifacts:
  - `forth/kernel.s32x`
  - `runtime/libc_debug.s32a`
  - `runtime/libc_mmio.s32a`
  - `runtime/libs32.s32a`
- Not tracked:
  - Host-native tool executables (for example `selfhost/stage00/s32-emu`,
    `tools/utilities/slow32dump`)
  - Rebuildable app/regression executables (`*.s32x` outputs)

Note: current checked-in binaries assume little-endian hosts. Big-endian
support is not currently a bootstrap target.

## Architecture Overview

- **32-bit RISC-like ISA** with 32 registers (r0 hardwired to zero)
- **No condition codes** - comparisons return 0/1 in GPR
- **Fixed 32-bit instructions**
- **W^X memory protection** - code segment is execute-only
- **Single-ported memory** (deliberately slow!)
- **DEBUG instruction** for character output; **MMIO ring buffers** for full I/O
- **Native floating point**: f32/f64 arithmetic on GPRs (f64 in even register pairs)
- **Sparse memory allocation** - Only allocates touched pages (99.4% memory savings!)
- **Graphics**: the **tube** service — three display modes (vector,
  framebuffer, PPU/tile), an MMIO wire protocol, and two host viewers;
  spec in [docs/TUBE.md](docs/TUBE.md)
- **Performance**: ~240 MIPS interpreted, ~1 BIPS under QEMU TCG, and up to
  ~8.3 BIPS under the dynamic binary translator (Apple M5 Max, 2026-08-23;
  4.07 BIPS on a virtualized Xeon) — see [docs/EMULATORS.md](docs/EMULATORS.md)

### Register Convention

- `r0`: Always zero
- `r1-r2`: Return values
- `r3-r10`: Function arguments
- `r11-r28`: General purpose
- `r29`: Stack pointer (sp)
- `r30`: Frame pointer (fp)
- `r31`: Link register (lr)

## Instruction Set

### Arithmetic (R-type)

- `add`, `sub`, `and`, `or`, `xor` - Basic arithmetic/logic
- `sll`, `srl`, `sra` - Shifts
- `mul`, `mulh`, `div`, `rem` - Multiplication/division

### Comparison (R-type)

- `slt`, `sltu` - Set less than (signed/unsigned)
- `seq` - Set equal (rd = rs1 == rs2 ? 1 : 0)
- `sne` - Set not equal (rd = rs1 != rs2 ? 1 : 0)
- `sgt`, `sgtu` - Set greater than (signed/unsigned)
- `sle`, `sleu` - Set less or equal (signed/unsigned)
- `sge`, `sgeu` - Set greater or equal (signed/unsigned)

### Immediate (I-type)

- `addi`, `ori`, `andi` - Immediate operations
- `slli`, `srli`, `srai` - Immediate shifts
- `slti`, `sltiu` - Set less than immediate
- `lui` - Load upper immediate

### Memory Operations

- `ldb`, `ldh`, `ldw` - Load byte/half/word (signed)
- `ldbu`, `ldhu` - Load byte/half (unsigned)
- `stb`, `sth`, `stw` - Store byte/half/word

### Control Flow

- `jal` - Jump and link
- `jalr` - Jump and link register
- `beq`, `bne` - Branch equal/not equal
- `blt`, `bge` - Branch less than/greater or equal
- `bltu`, `bgeu` - Branch unsigned comparisons

### Special Instructions

- `nop` - No operation
- `yield` - Waste cycles
- `debug` - Output character in rs1
- `halt` - Stop execution

## Assembler Directives

Following RISC-V/GNU conventions:

- `.text` / `.code` - Code section
- `.data` - Data section
- `.byte` - 8-bit values
- `.half` - 16-bit values
- `.word` - 32-bit values
- `.string` - Null-terminated string
- `.ascii` - String without null terminator
- `.global` - Global symbol

## Memory Layout

### Automatic Compact Mode
The linker automatically detects small programs and creates ultra-compact layouts:

- **Tiny programs**: As small as 9KB total (4KB code + 4KB heap + 1KB stack)
- **Page-aligned**: 4KB boundaries for hardware memory protection
- **Configurable**: Full control with `--code-size`, `--stack-size`, etc.

### Default Layout
| Address Range | Size | Description |
|--------------|------|-------------|
| 0x00000000 - 0x000FFFFF | 1MB | Code segment (execute-only) |
| 0x00100000 - 0x0FFFFFFF | 255MB | Data segment (read/write) |
| 0x10000000+ | - | MMIO region (link with `--mmio SIZE`, access via `__mmio_base`) |
| Stack: 0x0FFFFFF0 | - | Grows downward |

## Performance Characteristics

- Memory operations: 3 cycles
- Multiplication: 32 cycles
- Division/Remainder: 64 cycles
- All other operations: 1 cycle
- YIELD instruction: Variable cycle waste

## Project Structure

```
slow-32/
├── tools/              # Toolchain and host-side tooling
│   ├── emulator/       #   CPU emulators (slow32, slow32-fast), W^X protection
│   ├── assembler/      #   Two-pass assembler with relocation support
│   ├── linker/         #   Linker with symbol resolution
│   ├── utilities/      #   Binary analysis (slow32dump, slow32dis, s32-ar)
│   ├── dbt/            #   Dynamic binary translator (x86-64 and AArch64 JIT)
│   ├── dbt5/           #   Clean-room Stage-5 DBT fork
│   ├── s32-crt/        #   Tube viewer (terminal)
│   └── s32-crt-mac/    #   Tube viewer (native macOS)
├── common/             # Shared format definitions (.s32o, .s32x)
├── runtime/            # C runtime (crt0), intrinsics, standard library
│   └── include/        #   C standard library headers
├── selfhost/           # Self-hosting bootstrap chain (stage00..stage08 +
│                       #   cross-compilers targeting x86-64 and AArch64)
│
├── llvm-backend/       # LLVM backend for native clang/llc support
├── clang-target/       # Clang driver/target definitions
├── qemu-backend/       # QEMU TCG backend
├── fpc-backend/        # Free Pascal Compiler backend
├── fpga/               # RTL and simulation
│
├── doom/               # DOOM — framebuffer flagship, bit-exact timedemo
├── asteroids/          # Vector flagship, cross-engine determinism proof
├── ppu-reel/           # PPU conformance reel — 14 frozen frames
├── examples/           # Tube demos (fire, sprites) and sample programs
│
├── sqlite/             # SQLite
├── lua/                # Lua
├── zork/               # Z-machine
├── dbase/              # dBase III clone
├── sbasic/             # BASIC
├── forth/              # Forth kernel (SLOW-32 assembly)
├── forthc/             # AOT Forth compiler (written in Forth)
├── lisp/               # Lisp
├── prolog/             # Prolog
├── command/            # COMMAND.COM-shaped shell
├── clip/               # Clipper (.prg → .s32x)
├── sheet/              # Spreadsheet
├── nano/               # Editor
├── rogue/              # Rogue
├── bbs/                # BBS
├── net/                # IPv4 TCP
├── kermit/             # Kermit file transfer
│
├── regression/         # Regression suite + cross-engine differential harness
├── benchmarks/         # Benchmark programs
├── scripts/            # Helper scripts (compile.sh, test-quick.sh)
├── docker/             # Container build assets
├── articles/           # Substack write-ups
├── pitstop/            # Scratch notes: miscompile repros, optimization ideas
└── docs/               # Documentation
    ├── INSTRUCTION-SET.md    # Complete ISA reference
    ├── TUBE.md               # Graphics service specification
    ├── EMULATORS.md          # The five execution engines
    ├── CALLING_CONVENTION.md # ABI reference
    ├── file-formats.md       # Object and executable formats
    └── IMPROVEMENTS.md       # Known issues and improvements
```

## Example Programs

### Hello World (Assembly)
```asm
.data
    msg: .string "Hello, World!\n"

.text
.global _start
_start:
    lui r10, 0x100      # Load data segment base
    add r11, r10, r0    # Initialize pointer
    
print_loop:
    ldbu r12, r11+0     # Load character
    seq r13, r12, r0    # Check for null
    bne r13, r0, done   # Exit if null
    debug r12           # Print character
    addi r11, r11, 1    # Next character
    beq r0, r0, print_loop
    
done:
    halt
```

### C Example
```c
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    return factorial(5);  // Returns 120
}
```

## Current Status

- ✅ **Complete toolchain** - C → LLVM IR → Assembly → Object → Linked Executable
- ✅ **Native Clang target** - `-target slow32-unknown-none` (single dash)
- ✅ **All optimization levels** - -O0, -O1, -O2 fully working
- ✅ **Emulators** - five engines: two interpreters (~240 MIPS), the stage00
  bootstrap interpreter (~50 MIPS), a QEMU TCG target (~1 BIPS), and a DBT at
  ~8.3 BIPS (Apple M5 Max, 2026-08-23) — see [docs/EMULATORS.md](docs/EMULATORS.md)
- ✅ **Assembler** - Two-pass with labels, relocations, standard directives
- ✅ **Linker** - Symbol resolution, HI20/LO12 relocations, proper archives
- ✅ **LLVM backend** - PHI nodes, intrinsics, varargs, jump tables, 64-bit
  integers, native f32/f64, computed goto, C++ exceptions
- ✅ **Runtime** - crt0, buffered stdio, malloc, dual DEBUG/MMIO libc builds,
  64-bit builtins, CORDIC transcendentals, C++ EH runtime
- ✅ **Self-hosting** - `selfhost/` bootstraps from an 800-line emulator to a
  near-C99 compiler that rebuilds itself byte-identically, plus cross-compilers
  (x86-64, AArch64) that compile the project's own DBT
- ✅ **stage08 cc as a peer toolchain** - the self-hosted compiler now builds
  real applications end-to-end on SLOW-32: rogue, the graphics demos, DOOM
  (`-timedemo demo3` bit-exact with the clang build on both slow32-fast and
  slow32-dbt), and sbasic (output-identical to the clang build across its
  full test suite).  It shares clang's ABI — aligned-pair doubles and the
  byval struct-argument convention — with no known divergence, so stage08 and
  clang objects link and interoperate in both directions
  (`selfhost/stage08/run-interop-llvm.sh` gates it)
- ✅ **Graphics** - the tube service, all three modes landed: vector
  (`asteroids/`), framebuffer (`doom/`), and PPU/tile (`ppu-reel/`, spec frozen
  against a 14-frame conformance reel).  Golden-hash regression coverage,
  headless journaling via `S32_TUBE_DUMP`, two host viewers, and bindings in
  both SBASIC and Forth — see [docs/TUBE.md](docs/TUBE.md)
- ✅ **Applications** - DOOM, Rogue, Asteroids, SQLite, Lua, a Z-machine,
  dBase III clone, editor, BASIC, Forth (plus `forthc`, an AOT Forth compiler
  written in Forth), Lisp, Prolog, COMMAND.COM-shaped shell, Clipper,
  spreadsheet, BBS, Kermit, and IPv4 TCP examples under their own directories
- ✅ **Regression tests** - 79 tests in `regression/tests/`, plus
  `regression/run-differential.sh` which diffs every test across all execution
  engines.  C-based tests need an LLVM build; without clang the suite runs the
  assembly tests and skips the rest

## Known Limitations

- `scanf` and `fscanf` are declared but not implemented.  `sscanf` and
  `vsscanf` are implemented (`runtime/sscanf.c`), and are linked into the MMIO
  libc build
- `strftime` is minimal
- See `docs/IMPROVEMENTS.md` for open items and fixes

## Building from Source

```bash
# Prerequisites: make, gcc (plus LLVM with SLOW-32 backend for compiling C)

# Build everything
make

# Or build components individually
make emulator     # Builds slow32 and slow32-fast
make assembler    # Builds slow32asm
make linker       # Builds s32-ld
make utilities    # Builds slow32dump, slow32dis, s32-ar
make dbt          # Builds slow32-dbt (dynamic binary translator)
make runtime      # Builds crt0.s32o, libs32.s32a, libc_debug.s32a, libc_mmio.s32a
```

## Documentation

- [Instruction Set Reference](docs/INSTRUCTION-SET.md) - Complete ISA documentation
- [The Tube](docs/TUBE.md) - Graphics service specification (vec, fb, ppu)
- [Emulators](docs/EMULATORS.md) - The five execution engines, with measurements
- [Calling Convention](docs/CALLING_CONVENTION.md) - ABI reference
- [File Formats](docs/file-formats.md) - Object (.s32o) and executable (.s32x) formats
- [Improvements](docs/IMPROVEMENTS.md) - Known issues and suggested fixes
- [The 1987 Desk](docs/plans/1987-desk.md) - Period software we should (and should not) import
- [The hose](docs/plans/hose.md) - How two SLOW-32 programs talk (sockets, not shared memory)
- [Engine Room](docs/plans/engine-room.md) - DBT and codegen performance work
- [CLAUDE.md](CLAUDE.md) - AI assistant instructions and quick reference

## History

The first version of SLOW-32 went from specification to a working C compiler
in under two hours.  Everything above — the self-hosting bootstrap chain, the
second compiler, the graphics stack, DOOM — came after.

## License

Educational project - free to use for learning purposes.
