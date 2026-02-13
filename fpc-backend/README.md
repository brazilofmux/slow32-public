# SLOW-32 Free Pascal Backend

This directory contains the SLOW-32 backend for the Free Pascal Compiler (FPC), enabling compilation of Pascal programs to SLOW-32 assembly. The backend targets the `slow32-embedded` platform and produces `.s` files that can be assembled and linked with the SLOW-32 toolchain.

## Directory Structure

```
fpc-backend/
├── compiler/slow32/    # Backend source (code gen, register alloc, asm writer, etc.)
├── rtl/slow32/         # CPU-specific RTL (setjmp, atomics, stack intrinsics)
├── rtl/embedded/slow32/# Embedded startup code
├── runtime/            # SLOW-32 startup shim (fpc_startup.s)
├── examples/           # Example Pascal programs
├── patches/            # Integration patches for existing FPC files
├── scripts/            # Backup, restore, patch management, build-example.sh
└── README.md
```

## Quick Start

### Prerequisites

- A working FPC installation (3.2.2+ or trunk) that can compile for the host (e.g. x86_64-linux)
- The FPC source tree, cloned from https://gitlab.com/freepascal.org/fpc/source.git

### 1. Apply the backend to an FPC source tree

```bash
cd ~/slow-32/fpc-backend/scripts
./apply-patches.sh ~/fpc
```

This copies the SLOW-32 backend files and applies integration patches to register the new target in FPC's build system.

### 2. Build the cross-compiler

```bash
cd ~/fpc/compiler
./ppcx64 -dslow32 -Fuslow32 -Fusystems -Fislow32 \
  -Fu../rtl/units/x86_64-linux -oppcs32 pp.pas
```

This produces `ppcs32`, the SLOW-32 cross-compiler. It runs on x86_64 and emits SLOW-32 assembly.

### 3. Build the RTL (system unit)

```bash
cd ~/fpc/compiler
./ppcs32 -Tembedded -Us -s -n -Sg \
  -Fi../rtl/embedded -Fi../rtl/embedded/slow32 -Fi../rtl/inc \
  -Fi../rtl/inc/innr -Fi../rtl/slow32 -Fu../rtl/inc \
  -FE/tmp/slow32rtl ../rtl/embedded/system.pp
```

This compiles the system unit, producing:
- `/tmp/slow32rtl/system.ppu` — compiled unit (needed to compile user programs)
- `/tmp/slow32rtl/system.s` — assembly output for the runtime

### 4. Compile a Pascal program

```bash
./ppcs32 -Tembedded -s -n \
  -Fu/tmp/slow32rtl \
  -FE/tmp/slow32out program.pas
```

Output: `/tmp/slow32out/program.s`

### 5. Assemble, link, and run

The easiest way is to use the build script (RTL artifacts are checked into the repo):

```bash
cd ~/slow-32/fpc-backend/scripts
./build-example.sh ../examples/hello2.pas --run
```

Or manually:

```bash
FPC_RT=~/slow-32/fpc-backend/runtime
RT=~/slow-32/runtime

# Assemble startup shim and program (system.s32o is pre-assembled in-tree)
~/slow-32/tools/assembler/slow32asm $FPC_RT/fpc_startup.s fpc_startup.s32o
~/slow-32/tools/assembler/slow32asm /tmp/slow32out/program.s program.s32o

# Link (crt0 + FPC startup + system unit + program + MMIO libc + runtime)
~/slow-32/tools/linker/s32-ld --mmio 64K -o program.s32x \
  $RT/crt0.s32o fpc_startup.s32o $FPC_RT/system.s32o program.s32o \
  $RT/libc_mmio.s32a $RT/libs32.s32a

# Run
~/slow-32/tools/emulator/slow32-fast program.s32x
```

The link order matters: crt0 must be first (provides `_start`), then the FPC startup shim (provides `_haltproc`), then the system unit and program, then the C libraries.

> **Note:** The RTL artifacts (`system.s`, `system.ppu`, `system.s32o`) are checked into `fpc-backend/runtime/`. To rebuild them after modifying the FPC RTL source, re-run step 3 and copy the outputs back.

## Compiler Flags Reference

| Flag | Purpose |
|------|---------|
| `-Tembedded` | Target the embedded platform |
| `-Us` | Compile as the system unit (only for system.pp) |
| `-s` | Generate assembly only, don't invoke external assembler |
| `-n` | Don't read fpc.cfg |
| `-Sg` | Enable goto support |
| `-Fu<path>` | Unit search path (for finding system.ppu) |
| `-Fi<path>` | Include file search path |
| `-FE<path>` | Output directory for .s and .ppu files |

## Generated Assembly

The compiler outputs standard GAS-format assembly with SLOW-32 mnemonics. Example output for a simple program:

```asm
.section .text.n_main
    .balign 4
.globl  main
main:
    addi    r29,r29,-8       # allocate stack frame
    stw     r30,r29+4        # save frame pointer
    stw     r31,r29+0        # save link register
    addi    r30,r29,8        # set up frame pointer
    addi    r29,r29,-52      # allocate locals
    addi    r4,r0,42         # load constant 42
    la      r3,MY_VAR        # load address of global
    stw     r4,r3+0          # store to global
    ldw     r31,r29+52       # restore link register
    ldw     r30,r29+56       # restore frame pointer
    addi    r29,r29,60       # deallocate frame
    jalr    r0,r31           # return
```

Key characteristics:
- Memory references use `base+offset` format (e.g., `r29+4`)
- Function prologue saves r30 (fp) and r31 (lr), sets up frame pointer
- Constants loaded via `addi rd,r0,imm` (small) or `lui`+`ori` (32-bit)
- Globals accessed via `la` pseudo-instruction
- ELF-style section directives (`.section`, `.globl`, `.size`, `.balign`)

## Architecture

The backend consists of ~30 Pascal units in `compiler/slow32/`, forked from the RISC-V 32-bit backend and adapted for SLOW-32:

| Unit | Purpose |
|------|---------|
| `cpubase.pas` | Opcodes (TAsmOp), register definitions, assembler constants |
| `cgcpu.pas` | Main code generator — loads, stores, comparisons, branches |
| `cpupara.pas` | Calling convention — args in r3-r10, return in r1 |
| `ags32gas.pas` | GAS assembly output writer |
| `ras32gas.pas` | Inline assembler reader (for `asm...end` blocks) |
| `hlcgcpu.pas` | High-level code generation |
| `rgcpu.pas` | Register allocator |
| `ns32add.pas` | Arithmetic node lowering |
| `ns32cnv.pas` | Type conversion lowering |
| `ns32mat.pas` | Unary/pattern operations |
| `aoptcpu.pas` | Peephole optimizer |

The RTL (`rtl/slow32/`) provides CPU-specific implementations:
- Stack frame intrinsics (`get_frame`, `Sptr`, `get_caller_addr`)
- SetJmp/LongJmp for exception handling
- Soft-float FPU initialization (no hardware FPU)
- Non-atomic fallbacks for `InterlockedIncrement` etc. (single-core)
- Memory barriers (no-ops on single-core)

## Workflow (Development)

The FPC source tree at `~/fpc` is a read-only upstream clone. SLOW-32 changes are made there, then backed up to this repository.

### After making changes in ~/fpc:

```bash
cd ~/slow-32/fpc-backend/scripts
./backup.sh              # Copy backend source files
./generate-patches.sh    # Regenerate integration patches
cd ~/slow-32
git add fpc-backend/
git commit -m "Update FPC backend"
```

### Restoring to ~/fpc:

```bash
cd ~/slow-32/fpc-backend/scripts
./restore.sh             # Overwrites ~/fpc (interactive confirmation)
```

## Current Status

- ppcs32 cross-compiler builds and runs
- System unit (embedded RTL) compiles to .s + .ppu
- User programs compile to .s assembly output
- Generated assembly uses correct SLOW-32 instructions and conventions
- Full pipeline validated: Pascal -> ppcs32 -> slow32asm -> s32-ld -> slow32-fast
- "Hello, Pascal" test program runs correctly with exit code 0
- Soft-float only (no hardware FPU)

## Files Modified in FPC

The integration patches modify these existing FPC files to register the `slow32` target:

| Patch | Files |
|-------|-------|
| 01-compiler-build | `compiler/Makefile`, `compiler/Makefile.fpc` |
| 02-system-registration | `compiler/systems.inc`, `compiler/systems.pas`, `compiler/entfile.pas` |
| 03-compiler-defines | `compiler/fpcdefs.inc`, `compiler/globals.pas`, `compiler/options.pas`, `compiler/version.pas`, `compiler/cgbase.pas` |
| 04-target-registration | `compiler/systems/i_embed.pas`, `compiler/systems/t_embed.pas` |
| 05-compiler-driver | `compiler/pp.pas`, `compiler/psystem.pas`, `compiler/dbgdwarf.pas`, `compiler/utils/fpc.pp`, `compiler/utils/ppuutils/ppudump.pp` |
| 06-rtl-build | `rtl/Makefile`, `rtl/embedded/Makefile`, `rtl/embedded/Makefile.fpc` |
| 07-rtl-system | `rtl/inc/systemh.inc`, `rtl/inc/system.inc` |
