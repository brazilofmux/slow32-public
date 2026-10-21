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

On an Apple Silicon Mac use `scripts/build-compiler.sh` instead: it builds in
two stages (3.2.2 -> trunk `ppca64` + trunk RTL for aarch64-darwin -> `ppcs32`).
A `ppcs32` built directly by brew's FPC 3.2.2 is miscompiled -- it crashed
with "Compilation raised exception internally" on `with r^ do begin ... exit
end`, died with EBusError on varutils.pp and could not compile variants.pp or
rtti.pp; the same sources built by the trunk host compiler have none of this
(found by the FPC test-suite run, 2026-10-10).

### 3. Build the RTL (system unit)

```bash
cd ~/fpc/compiler
./ppcs32 -Tembedded -Us -s -n -Sg \
  -Fi../rtl/embedded -Fi../rtl/embedded/slow32 -Fi../rtl/inc \
  -Fi../rtl/inc/innr -Fi../rtl/slow32 -Fu../rtl/inc \
  -FE/tmp/slow32rtl ../rtl/slow32/system.pp
```

Note: build from `../rtl/slow32/system.pp` (the SLOW-32 fork), not
`../rtl/embedded/system.pp`. The fork wires in SysInitMemoryManager,
SysInitStdIO and Slow32InitCmdLine — without them WriteLn fails with
runtime error 103.

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

### 6. The rest of the RTL, and the Free Pascal test suite

`scripts/build-rtl.sh` builds the whole embedded RTL for SLOW-32 the way
rtl/embedded's Makefile lists it (sysutils, classes, math, typinfo, ...),
plus variants, strutils, dateutils, rtti and heaptrc, into
`runtime/units/` (not in git) as `fpcrtl.s32a` -- link it after the
program.  `scripts/run-fpc-tests.sh` then runs Free Pascal's own test suite
(`~/fpc/tests/{test,tbs,webtbs}`, about 5,200 programs) through ppcs32,
slow32asm, s32-ld and the emulator and sorts the outcomes (PASS,
COMPILE-FAIL with the message, RUN-FAIL(rc), RUN-FAULT, TIMEOUT, SKIP by
directive).  Tests link with `--stack-size 1M --heap-size 32M`.

First run (2026-10-10): 3,630 of 5,203 passed; at the end of the day 4,148,
with 900 skipped by directive and 155 failing.  What the suite found, all
fixed in ~/fpc and mirrored here:

| Where | Defect |
|-------|--------|
| host build | ppcs32 built by FPC 3.2.2 on aarch64-darwin was miscompiled (see step 2) |
| rtl/slow32/system.pp | `initunicodestringmanager` was never called: every ansistring -> unicodestring conversion called address 0 (= `_start`) and the program restarted inside itself until the stack was gone |
| rtl/embedded/sysutils.pp | pulled in heapmgr, which replaced the libc_mmio heap with a static block (runtime error 203, hangs in heapmgr's free list) |
| rtl/slow32/slow32.inc | `get_frame`, `Sptr` and the five atomics returned in r3, not r1: the exception unwinder walked a bogus frame, `InterlockedIncrement` never settled |
| rtl/slow32/slow32.inc | `get_caller_frame`/`get_caller_addr` dereferenced a nil frame; and the backend saved registers in number order, so lr/fp were not at the fixed offsets they read (`g_proc_entry` now saves lr, fp first) |
| rtl/slow32/mathu.inc | missing: math.pp did not compile, nor types, fgl, classes |
| rtl/inc/softfpu.pp (SLOW-32 only) | ordered compares raised EInvalidOp on any NaN; hardware FPC targets raise only on a signalling NaN (tminmax1) |
| compiler/slow32/ns32mat.pas | NOT of a QWordBool left the high half unallocated (`stw r4,INVALID,4`) |
| compiler/slow32/cgcpu.pas | no 64-bit add/sub overflow checking (tint642) |
| compiler/nadd.pas | `currency * 2` became a shift whose constant was typed Currency, then forced to LongInt (tw41865) |

Residue (155): 23 heap exhaustions (heaptrc-using tests and tests that
allocate more than the harness's 32M), 15 deep-recursion / stack-check
tests, 18 tests failing their own checks, 10 unhandled exceptions (file
tests mostly), 42 tests wanting units we do not build (`cthreads`, `cmem`,
`crt`, `sockets`, `objects`, `Generics.Collections`), hardware-trap tests
(`tabsvr6`), and the suite's own quirks (`%OPT` with a backslash, `-k`
linker options we never see, generic front-end items that fail on the host
compiler too).  `OUT=/tmp/fpc-tests-slow32` holds `results.tsv` and each
failure's work directory.

### 7. The ABI tests

`tests/cdecl-abi/run.sh` compiles a Pascal program with ppcs32 and a C file
with clang and links them: every `cdecl` parameter shape that tells the
conventions apart (a double after an int, an int64 after the hole that
leaves, a double or int64 at r10, fixed and variadic arguments of a varargs
function), in both directions, with the C side printing what arrived.
`tests/shift64/run.sh` checks 64-bit shifts by a variable count against what
a host Free Pascal prints.  `tests/frametemps/run.sh` (-O2 -CfSLOW32, the FPU
RTL) keeps more than thirteen callee-saved registers live across a call and
checks the caller's values survive: frame temporaries must lie below the
register save area.  All three must say PASS after a compiler change; none
is part of the FPC suite above.

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
- Soft-float FPU initialization for the default `fpu_soft` mode (the
  `-CfSLOW32` mode uses the hardware FP instructions; see FPU notes below)
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
- Native f32 **and** f64 under `-CfSLOW32`:
  - f32 lives in F-class regs (FADD.S / FCVT / FSQRT / compares / …)
  - f64 uses even int pairs + `F*.D` via scratch even-pair helpers
    (`a_alloc_evenpair` / `a_load_reg64_evenpair` in `cgcpu.pas`)
- Default mode remains full soft-float (`fpu_soft`); units must match the
  program's `-Cf` mode (RTL ships both: `runtime/` and `runtime/fpu/`)
- ABI: floats still cross calls in integer registers either way
- Float tests: 18-check fputest + 37-check regtest pass in both modes
  (see satellite `~/fpc` workflow / agent handoff notes)

### FPU notes

- Compiler-side F-class registers alias the callee-saved GPRs r20–r28; the
  int allocator gives those up, so the pools never collide and emitted asm
  uses plain rN names.
- f64 ISA ops require an even base register (hi = base+1); HL codegen
  stages pairs through fixed even scratch bases before/after the op.
- Unsigned 32-bit `div`/`mod` route through `fpc_div_dword`/`fpc_mod_dword`
  (the ISA only has signed DIV/REM); smaller unsigned types stay on native
  DIV/REM via zero-extension.

Build for the FPU: `./scripts/build-example.sh program.pas --run --fpu`

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
| 08-compiler-fpu | `compiler/defutil.pas`, `compiler/symdef.pas`, `compiler/nld.pas` |
