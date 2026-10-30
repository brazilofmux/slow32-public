# SLOW-32 Project

A deliberately inefficient 32-bit RISC CPU architecture with complete toolchain.

- **Purpose**: Educational CPU design and sandboxed compute engine
- **Status**: Active. Two independent C compilers (an LLVM backend and a
  self-hosted one that share an ABI), COBOL and Fortran 77 compilers, five
  execution engines, a graphics stack, and a broad application corpus — see [Current Status](#current-status)

## Quick Start

The quickest way in is a container; nothing to build:

```bash
docker run --rm -v "$PWD":/data -w /data hurriedreformist/slow32:toolchain \
    sh -c 'clang --target=slow32-unknown-none -O2 hello.c -o hello.s32x && s32run hello.s32x'
```

See [Containers](#containers) for the other images.  From a checkout,
with LLVM built from `llvm-backend/` into `~/llvm-project/build`:

```bash
# Build the assembler, linker, emulators, DBT and runtime
make

# C to executable in one step: the clang driver assembles and links
# (crt0, libc_mmio, libs32) when SLOW32_HOME names the tree
SLOW32_HOME=$PWD ~/llvm-project/build/bin/clang --target=slow32-unknown-none -O2 program.c -o program.s32x
./tools/emulator/slow32 program.s32x

# Or step by step
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x
```

## Containers

The images form a chain, each `FROM` the one above it, and are published
for amd64 and arm64 as `hurriedreformist/slow32:<variant>` on Docker Hub
(`Dockerfile.*` at the top of the tree builds them locally):

| Image | Adds |
|---|---|
| `emulator` | the four engines (`slow32`, `slow32-fast`, `slow32-dbt`, `qemu-system-slow32`) and `s32run` |
| `base` | assembler, linker, librarian, dump/disassembler, runtime archives and headers |
| `toolchain` | LLVM/clang, libc++/libc++abi and `s32c++`, Free Pascal `ppcs32`, the cc-x64/cc-a64 cross-compilers |
| `cobol` | `s32-cobc`, libcob, and the `s32cob` driver |
| `fortran` | `f77`, libf77, and the `s32f77` driver |
| `forth` | the Forth kernel and `s32forth`; `forthc` as `s32forthc` |
| `kit` | the self-hosted toolchain (stage08 cc, as, ld, ar as `.s32x`) and `s32kcc` |

## Binary Artifact Policy

Some SLOW-32 binaries are intentionally checked into git because they are
bootstrap assets and host-agnostic across little-endian machines.

- Tracked bootstrap artifacts:
  - `forth/kernel.s32x`
  - `runtime/crt0.s32o`, `runtime/locales.s32o`
  - `runtime/libc_debug.s32a`, `runtime/libc_mmio.s32a`, `runtime/libs32.s32a`,
    `runtime/libs32cxx.s32a`
- Also tracked, ready-to-run builds of some apps: `doom`,
  `dbase`, `lua`, `nano`, `prolog`, `sbasic`, `zork`, `ppu-reel/reel` and
  `forthc/demo/ship` (`.s32x`)
- Not tracked:
  - Host-native tool executables (for example `selfhost/stage00/s32-emu`,
    `tools/utilities/slow32dump`)
  - Other rebuildable app and regression executables (`*.s32x` outputs)

Note: current checked-in binaries assume little-endian hosts. Big-endian
support is not currently a bootstrap target.

## Architecture Overview

- **32-bit RISC-like ISA** with 32 registers (r0 hardwired to zero)
- **No condition codes** - comparisons return 0/1 in GPR
- **Fixed 32-bit instructions**
- **W^X memory protection** - code and read-only data cannot be written,
  and instructions are fetched only from the code region
- **Single-ported memory** (deliberately slow)
- **DEBUG instruction** for character output; **MMIO ring buffers** for full
  I/O (files, arguments, environment, time, sockets and more)
- **Native floating point**: f32/f64 arithmetic on GPRs (f64 in even register pairs)
- **Sparse memory** - engines allocate only the pages a program touches
- **Graphics**: the **tube** service — three display modes (vector,
  framebuffer, PPU/tile), an MMIO wire protocol, and two host viewers;
  spec in [docs/TUBE.md](docs/TUBE.md)
- **Performance** (Apple M5 Max): ~240 MIPS in the reference interpreter,
  ~500 in the pre-decoding one, ~4 BIPS under QEMU TCG, and up to ~8.3 BIPS
  under the dynamic binary translator (4.07 BIPS on a virtualized Xeon) — see [docs/EMULATORS.md](docs/EMULATORS.md)

### Register Convention

- `r0`: Always zero
- `r1-r2`: Return values
- `r3-r10`: Function arguments
- `r11-r28`: General purpose
- `r29`: Stack pointer (sp)
- `r30`: Frame pointer (fp)
- `r31`: Link register (lr)

## Instruction Set

[docs/SPEC.md](docs/SPEC.md) is the normative machine specification:
every instruction, the executable format, loading, faults, and the host
interface, complete enough to build an engine from.
[docs/INSTRUCTION-SET.md](docs/INSTRUCTION-SET.md) is the assembler-level
reference.  In brief:

- **Register-register:** `add sub and or xor sll srl sra mul mulh mulhu div rem`
- **Compare (0/1 into a register):** `slt sltu seq sne sgt sgtu sle sleu sge sgeu`
- **Immediate:** `addi andi ori xori slli srli srai slti sltiu`, and `lui`
- **Memory:** `ldb ldbu ldh ldhu ldw`, `stb sth stw` (base + 12-bit offset)
- **Control:** `jal jalr`, `beq bne blt bge bltu bgeu` (the assembler also
  takes `bgt ble bgtu bleu`, operands swapped)
- **Floating point:** `fadd fsub fmul fdiv fsqrt feq flt fle fneg fabs` in
  `.s` and `.d`, and conversions between f32, f64 and 32/64-bit integers
- **System:** `nop`, `yield` (a host-interface service point), `debug`
  (print the character in rs1), `assert_eq`, `halt`

No timing is architectural.  `slow32-fast` reports simulated cycles: one
per instruction, four for a load or store, 32 for a multiply, 64 for a
divide or remainder.

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

The address space is 256 MB.  The linker lays the regions out from 0,
each 4 KB aligned:

| Region | Where | Access |
|---|---|---|
| Code | from 0 (entry point 0) | fetch, read |
| Read-only data (`.rodata`, `.eh_frame`) | after the code | read |
| Data, BSS, heap | after the read-only data | read, write |
| MMIO window (64 KB, when linked `--mmio`) | after the heap; found through `__mmio_base` | read, write |
| Stack | top at 0x0FFFFFF0, growing down (64 KB by default) | read, write |

`s32-ld` sets the limits (`--code-size`, `--rodata-size`, `--data-size`,
`--heap-size`, `--stack-size`; 1 MB each by default except the stack) and
records them in the executable's header, where the engines read them.
The details are in [docs/SPEC.md](docs/SPEC.md) section 6.

## Project Structure

```
slow-32/
├── tools/              # Toolchain and host-side tooling
│   ├── emulator/       #   Reference interpreter (slow32) and slow32-fast
│   ├── assembler/      #   Two-pass assembler with relocation support
│   ├── linker/         #   Linker with symbol resolution
│   ├── utilities/      #   Binary analysis (slow32dump, slow32dis, s32-ar)
│   ├── dbt/            #   Dynamic binary translator (x86-64 and AArch64 JIT)
│   ├── dbt5/           #   Clean-room Stage-5 DBT fork
│   ├── lifter/         #   s32-lift: .s32x to LLVM IR
│   ├── s32-crt/        #   Tube viewer (terminal)
│   └── s32-crt-mac/    #   Tube viewer (native macOS)
├── common/             # Shared format definitions (.s32o, .s32x)
├── runtime/            # C runtime (crt0), intrinsics, standard library
│   └── include/        #   C standard library headers
├── selfhost/           # Self-hosting bootstrap chain (stage00..stage08 +
│                       #   cross-compilers targeting x86-64 and AArch64)
├── fortran/            # Fortran 77 compiler (f77 → SLOW-32 assembly)
├── cobol/              # COBOL compiler (s32-cobc) and its runtime, libcob
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
├── pcre2/              # PCRE2
├── dfsort/             # Sort utility (s32sort)
├── lua/                # Lua
├── zork/               # Z-machine
├── dbase/              # SLOW-32 xBase: the interpreter (dot prompt)
├── sbasic/             # BASIC
├── forth/              # Forth kernel (SLOW-32 assembly)
├── forthc/             # AOT Forth compiler (written in Forth)
├── islisp/             # ISLISP (ISO Lisp)
├── prolog/             # Prolog
├── command/            # COMMAND.COM-shaped shell
├── clip/               # SLOW-32 xBase: the compiler (.prg → .s32x)
├── sheet/              # Spreadsheet
├── nano/               # Editor
├── rogue/              # Rogue
├── bbs/                # BBS
├── net/                # IPv4 TCP
├── kermit/             # Kermit file transfer
│
├── regression/         # Regression suite, differential harnesses, fuzzers
├── benchmarks/         # Benchmark programs
├── scripts/            # Helper scripts (compile.sh, test-quick.sh)
├── docker/             # Container build assets (the s32* drivers)
├── articles/           # Substack write-ups
├── pitstop/            # Scratch notes: miscompile repros, optimization ideas
└── docs/               # Documentation
    ├── SPEC.md               # The machine specification (normative)
    ├── INSTRUCTION-SET.md    # Assembler-level ISA reference
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
msg:    .string "Hello, World!\n"

        .text
        .global _start
_start:
        lui  r11, %hi(msg)          # address of msg
        addi r11, r11, %lo(msg)
loop:
        ldbu r12, r11+0             # next character
        beq  r12, r0, done          # stop at the NUL
        debug r12                   # print it
        addi r11, r11, 1
        jal  r0, loop
done:
        halt
```

```bash
./tools/assembler/slow32asm hello.s hello.s32o
./tools/linker/s32-ld -o hello.s32x hello.s32o
./tools/emulator/slow32 hello.s32x
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

- ✅ **Toolchain** - C → LLVM IR → assembly → object → linked executable,
  with a proper linker and archives; the clang driver runs the whole chain
  (`clang --target=slow32-unknown-none prog.c -o prog.s32x`)
- ✅ **LLVM backend** - -O0 through -O2; PHI nodes, intrinsics, varargs,
  jump tables, 64-bit integers, native f32/f64, computed goto, tail calls,
  shrink-wrapping.  `regression/run-llvm-verify.sh` runs LLVM's machine
  verifier over every C library clang builds for SLOW-32
- ✅ **C++** - exceptions, RTTI, and libc++/libc++abi (iostreams, fstream,
  filesystem, named locales); `s32c++` in the toolchain image
- ✅ **Free Pascal** - a `ppcs32` backend in `fpc-backend/`, run against
  FPC's own test suite
- ✅ **Emulators** - five engines: the reference interpreter (~240 MIPS),
  slow32-fast (~500 MIPS), the stage00 bootstrap interpreter (~24 MIPS), a
  QEMU TCG target (~4 BIPS), and a DBT at ~8.3 BIPS (Apple M5 Max) — see [docs/EMULATORS.md](docs/EMULATORS.md)
- ✅ **Runtime** - crt0, buffered stdio, malloc, dual DEBUG/MMIO libc builds,
  64-bit builtins, CORDIC transcendentals, locales, C++ EH runtime
- ✅ **Self-hosting** - `selfhost/` bootstraps from a ~950-line emulator to a
  C11 compiler that rebuilds itself byte-identically, plus cross-compilers
  (x86-64, AArch64) that compile the project's own DBT.  Every valid probe of
  the C11 survey passes (`selfhost/c-conformance/`: 320 PASS, 0 WRONG,
  0 REJECTS), VLAs included; complex types are left out by choice, and
  diagnostics follow clang's defaults
- ✅ **stage08 cc as a peer toolchain** - the self-hosted compiler builds
  real applications end-to-end on SLOW-32: SQLite (output byte-identical to
  the clang build, `sqlite/check-stage08.sh`), DOOM (`-timedemo` bit-exact
  with the clang build), rogue, the graphics demos, sbasic, and the ISLISP
  interpreter.  It shares clang's ABI, so stage08 and clang objects link and
  interoperate in both directions (`selfhost/stage08/run-interop-llvm.sh`)
- ✅ **COBOL** - `cobol/` (`s32-cobc`) implements COBOL through the 2023
  standard; 85, where the work began, is the default `-std`, and 2002, 2014
  and 2023 are a switch away; all 348 NIST CCVS-85 programs compile
  and run with no failures; embedded SQL on SQLite and PostgreSQL — see
  [cobol/README.md](cobol/README.md)
- ✅ **Fortran 77** - `fortran/` is a one-pass F77 compiler with its own
  SSA/BURG backend; it passes NIST's FCVS (192 of 192) and is
  differential-tested against gfortran — see [fortran/README.md](fortran/README.md)
- ✅ **Graphics** - the tube service, all three modes landed: vector
  (`asteroids/`), framebuffer (`doom/`), and PPU/tile (`ppu-reel/`, spec frozen
  against a 14-frame conformance reel).  Golden-hash regression coverage,
  headless journaling via `S32_TUBE_DUMP`, two host viewers, and bindings in
  both SBASIC and Forth — see [docs/TUBE.md](docs/TUBE.md)
- ✅ **Applications** - DOOM, Rogue, Asteroids, SQLite, Lua, PCRE2, a
  Z-machine, an xBase interpreter and compiler, editor, BASIC, Forth (plus `forthc`, an AOT
  Forth compiler written in Forth), ISLISP (passes the eisl verification
  suite), Prolog, COMMAND.COM-shaped shell, spreadsheet, sort, BBS,
  Kermit, and IPv4 TCP examples under their own directories
- ✅ **Testing** - 138 tests in `regression/tests/`, run on every engine
  by `regression/run-differential.sh`; further differentials run the
  engines over stage08-built code (`run-kit-differential.sh`) and over the
  self-hosted tools (`run-kit-tools-differential.sh`), compare two compilers
  (`run-stage07-differential.sh`), and compare the two C libraries with the
  host's (`run-libc-differential.sh`); two fuzzers generate random C
  (`regression/fuzz/cfuzz.py`, stage08 against clang) and random machine
  code (`regression/fuzz/isafuzz.py`, every engine against the reference
  interpreter).  [CLAUDE.md](CLAUDE.md) lists the main ones with when to
  run each, and [regression/README.md](regression/README.md) the rest

## Known Limitations

- `scanf` and `fscanf` are declared but not implemented.  `sscanf` and
  `vsscanf` are implemented (`runtime/sscanf.c`), and are linked into the MMIO
  libc build
- See `docs/IMPROVEMENTS.md` for open items and fixes

## Building from Source

```bash
# Prerequisites: make, gcc (plus LLVM with SLOW-32 backend for compiling C)

# Build everything
make

# Or build components individually
make emulator     # Builds slow32 and slow32-fast
make s32-crt      # Builds the terminal tube viewer
make assembler    # Builds slow32asm
make linker       # Builds s32-ld
make utilities    # Builds slow32dump, slow32dis, s32-ar
make dbt          # Builds slow32-dbt (dynamic binary translator)
make runtime      # Builds crt0.s32o, locales.s32o, libs32.s32a, libc_debug.s32a, libc_mmio.s32a, libs32cxx.s32a
```

## Documentation

- [Machine Specification](docs/SPEC.md) - The normative spec: ISA, executable format, memory map, host interface
- [Instruction Set Reference](docs/INSTRUCTION-SET.md) - Assembler-level ISA documentation
- [The Tube](docs/TUBE.md) - Graphics service specification (vec, fb, ppu)
- [Emulators](docs/EMULATORS.md) - The five execution engines, with measurements
- [Calling Convention](docs/CALLING_CONVENTION.md) - ABI reference
- [File Formats](docs/file-formats.md) - Object (.s32o) and executable (.s32x) formats
- [Improvements](docs/IMPROVEMENTS.md) - Known issues and suggested fixes
- [The 1987 Desk](docs/plans/1987-desk.md) - Period software we should (and should not) import
- [The hose](docs/plans/hose.md) - How two SLOW-32 programs talk (sockets, not shared memory)
- [Engine Room](docs/plans/engine-room.md) - DBT and codegen performance work
- [COBOL](cobol/README.md), [Fortran 77](fortran/README.md), [Forth](forth/README.md), [Self-hosting](selfhost/README.md) - the larger fronts each have their own README, as do the interpreters: [xBase](dbase/README.md) (and its compiler, [s32-clip](clip/README.md)), [SLOW BASIC](sbasic/README.md), [Lua](lua/README.md), [Prolog](prolog/README.md), [ISLISP](islisp/README.md)
- [CLAUDE.md](CLAUDE.md) - AI assistant instructions and quick reference, including the main test harnesses

## History

The first version of SLOW-32 went from specification to a working C compiler
in under two hours.  Everything above — the self-hosting bootstrap chain, the
second compiler, the graphics stack, DOOM — came after.

## License

Original work in this repository — the ISA, toolchain, self-hosted
compiler, COBOL compiler, and everything else written here by
Stephen Dennis — is under the **MIT License**. See [LICENSE](LICENSE).

“Educational” in the purpose line above is what the project is for.
It is not a separate license. MIT already covers use, modification,
redistribution, and commercial use.

The backends written into other projects take those projects' terms:
`llvm-backend/` and `clang-target/` are Apache-2.0 with LLVM Exceptions,
`qemu-backend/` builds into QEMU under the GPL version 2, and
`fpc-backend/` is GPL (compiler) and Free Pascal's modified LGPL
(run-time library).

Third-party files keep their own terms; the notices live in
[NOTICE](NOTICE). David M. Gay’s `runtime/dtoa.c` (Lucent,
1991/2000/2001) is free to use, copy, modify, and distribute for any
purpose without fee, provided that entire notice is kept with copies. PCRE2 (`pcre2/`) is BSD-3-Clause with the
PCRE2 exception. Lua (`lua/src`) is MIT from Lua.org;
the SQLite amalgamation is public domain; MojoZork (`zork/src`) is
zlib; `islisp/lib/test.lsp` is adapted from Easy-ISLisp (BSD-2); the
Unicode and CLDR data the COBOL runtime, C runtime and `common/` carry
(through libutf) is under the Unicode License V3.  Doom under `doom/` (doomgeneric) is GPLv2 and stands alone:
it is a separate program, and nothing under MIT here links against it.

## Trademarks

dBASE and Clipper (CA-Clipper) are trademarks of their respective owners.
SLOW-32 xBase is not affiliated with or endorsed by them; the names appear
here only to say which programs and files it is compatible with.
