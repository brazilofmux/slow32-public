# CLAUDE.md - Quick Reference for AI Assistants

SLOW-32 is a 32-bit RISC CPU with complete toolchain. This file contains quick commands and context for AI assistance.

## Quick Commands

```bash
# Build everything
make

# Quick test of toolchain
./scripts/test-quick.sh

# Compile C program with helper script
./scripts/compile.sh program.c [output.s32x]

# Compile and run C program (manual method - native SLOW32 target)
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x

# Alternative: Standalone compiler (DEPRECATED - DO NOT USE)
# The standalone compiler in compiler/ is deprecated and should not be used.
# It cannot properly handle varargs and other complex C features.
# Always use the native LLVM backend method above.

# Debug mode
./tools/emulator/slow32 program.s32x -s  # step through each instruction
./tools/emulator/slow32 -t program.s32x  # trace every instruction
./tools/emulator/slow32 -r program.s32x  # show register changes
./tools/emulator/slow32 -c 1000 program.s32x  # limit to 1000 cycles
./tools/emulator/slow32 -b 0x100 program.s32x  # break at address
./tools/emulator/slow32 -w 0x1000-0x2000 program.s32x  # watch memory range
```

## Key Architecture Points

- 32 registers: r0=0, r1-r2=return, r3-r10=args, r29=sp, r30=fp, r31=lr
- No condition codes - comparisons return 0/1 in registers
- W^X protection - code segment is execute-only (0x0-0xFFFFF)
- Stack at 0x0FFFFFF0 growing down
- DEBUG instruction outputs character; MMIO ring buffers for full I/O (files, args, env)
- Five emulators from ~50 MIPS to ~8.3 BIPS (M5 Max, 2026-08-23) — see [docs/EMULATORS.md](docs/EMULATORS.md)

## Working Features

✅ Complete toolchain with proper linker (not concatenation!)  
✅ Object files (.s32o) with relocations and symbol tables  
✅ Executable format (.s32x) with resolved symbols  
✅ All comparison instructions (SEQ, SNE, SGT/U, SLE/U, etc.)  
✅ PHI node support for SSA form  
✅ LLVM intrinsics: memcpy, memset, lifetime, smax/smin  
✅ Varargs fully working with clang SLOW32 target!  
✅ Jump tables for switch statements (assembler, linker, LLVM backend all support)  
✅ Advanced relocations: %hi(symbol+offset), %lo(symbol+offset), %pcrel_hi, %pcrel_lo  
✅ 64-bit integers: FULLY COMPLETE including:

  - ADD/SUB with carry/borrow
  - MUL via UMUL_LOHI/SMUL_LOHI custom lowering
  - DIV/REM via compiler-rt libcalls (__divdi3, __udivdi3, __moddi3, __umoddi3)
  - All logical operations (AND, OR, XOR, shifts)
  - All comparison operations (EQ, NE, LT, GT, LE, GE for both signed/unsigned)
  - Comprehensive regression test coverage

✅ Native Clang target: `-target slow32-unknown-none` (note: single dash)  
✅ XORI instruction (opcode 0x1E) for XOR immediate operations
✅ LLVM backend updated for latest LLVM API (Sep 2025)
✅ Regression tests: ALL 62/62 PASSING (plus cross-engine differential harness)
✅ Runtime libraries built as archives: libs32.s32a (6KB), libc_debug.s32a (44KB), libc_mmio.s32a (72KB)
✅ Optimization passes fixed - no more LLC hangs
✅ MMIO support: Emulators use MMIO base from executable header, linker provides __mmio_base symbol
✅ Fixed emulator MMIO bug: no longer treats address 0x0-0xFFFF as MMIO when mmio_base=0
✅ Fixed LLVM backend stack argument bug: arguments 9+ now correctly accessed at fp+0, fp+4, etc.

✅ Runtime builds successfully with -O0, -O1, and -O2
✅ Some complex files (printf.c, stdio.c, etc.) use -O1 to avoid optimizer performance issues

⚠️ See docs/IMPROVEMENTS.md for known issues


## Important Notes for Toolchain

- **Always use the linker** - Never concatenate .s files!
- **crt0.s32o must be explicitly linked first** - Contains _start at address 0 (not included in archives)
- **Link order**: crt0.s32o, program.s32o, libc_debug.s32a (or libc_mmio.s32a), libs32.s32a
- **Use -O2** - Default optimization level, -O1 also works
- **Archives available**: libs32.s32a (runtime intrinsics + 64-bit builtins), libc_debug.s32a (DEBUG I/O), libc_mmio.s32a (MMIO ring buffer I/O)
- **MMIO support**: Use `--mmio SIZE` flag with linker (e.g., `--mmio 64K`), access via __mmio_base symbol

## Working C Examples

```c
// Printf with varargs
void test_printf() {
    printf("Hello, World!\n");
    printf("Number: %d\n", 42);
}

// Recursion
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// Loops with PHI nodes
int sum_to_n(int n) {
    int sum = 0;
    for (int i = 1; i <= n; i++) {
        sum += i;
    }
    return sum;
}
```

## Container Support (podman/docker)

Containers provide a clean baseline for testing. Two images, repo:tag
naming (`slow32:toolchain`, `slow32:emulator`) — the same names
~/builder's jobs build per-arch and the ECR mirror serves. Podman is
the local engine of choice; the commands below work with `docker`
substituted 1:1.

- `slow32:toolchain`: Full development environment with LLVM 22
- `slow32:emulator`: Lightweight runtime for testing executables (includes slow32, slow32-fast, qemu-system-slow32)

```bash
# Build images (if not already built)
podman build -t slow32:toolchain -f Dockerfile.toolchain .
podman build -t slow32:emulator -f Dockerfile.emulator .

# Run programs with the emulator container (easy wrapper script;
# auto-detects podman/docker and falls back to the legacy
# slow32-emulator image name if that's what is present)
./scripts/run-in-docker.sh program.s32x              # Use default slow32 emulator
./scripts/run-in-docker.sh --fast program.s32x      # Use optimized slow32-fast
./scripts/run-in-docker.sh --qemu program.s32x      # Use QEMU TCG emulator
./scripts/run-in-docker.sh -t program.s32x          # Trace mode (C++ emulators only)
./scripts/run-in-docker.sh --help                   # Show all options

# Manual commands (if you prefer)
podman run --rm -v $(pwd):/data slow32:emulator s32run program.s32x
podman run --rm -v $(pwd):/data slow32:emulator s32run --fast program.s32x
podman run --rm -v $(pwd):/data slow32:emulator s32run --qemu program.s32x

# Test against clean baseline when unsure if changes broke something
podman run --rm -v $(pwd):/workspace slow32:toolchain bash -c "cd /workspace && make"

# Run tests in clean environment
podman run --rm -v $(pwd):/workspace slow32:toolchain bash -c "cd /workspace/regression && ./run-tests.sh"

# Test specific program compilation (uses clang/llc from /usr/local/bin in container)
podman run --rm -v $(pwd):/workspace slow32:toolchain bash -c "cd /workspace && clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include test.c -o test.ll && llc -mtriple=slow32-unknown-none test.ll -o test.s"
```

## Cross-Compiler (stage08-cross-x64) — Build, Test, Benchmark

The cross-compiler (`selfhost/stage08-cross-x64/`) compiles C to native x86-64 ELF.
It pulls its C frontend (parser/sema/HIR/etc.) via symlinks from `../stage08/`.
It has its own Makefile. **Always use `--hir` when compiling with cc-x64** (the Makefile does this).

```bash
cd selfhost/stage08-cross-x64

# Build everything: cc-x64 (compiler) + s32fast-hir (emulator compiled by cc-x64)
make

# Run all tests (23 tests)
make test

# Benchmark: compare cross-compiled emulator vs GCC-compiled emulator
make bench                    # runs s32fast-hir on benchmark_core.s32x
time tools/emulator/slow32-fast ~/s32x/benchmark_core.s32x   # GCC baseline

# Rebuild after changing hir_regalloc_x64.h or hir_codegen_x64.h:
# (Makefile tracks deps — just `make` rebuilds cc-x64 then recompiles s32fast)
make

# Manual compilation (if needed):
out/cc-x64 --hir -c somefile.c -o somefile.o     # compile to .o
gcc -nostdlib -static -o binary out/crt0.o somefile.o out/libc_x64.a  # link

# Compare codegen quality (inspect hot function):
# NOTE: disassemble the .o — the linked out/s32fast-hir has no section headers
objdump -d out/s32fast-hir.o | sed -n '/<h_add>:/,/ret/p'    # our code
objdump -d tools/emulator/slow32-fast | sed -n '/<op_add>:/,/ret/p'  # gcc code

# Key benchmark: ~/s32x/benchmark_core.s32x (full 10M iters, 285M instructions)
# Expected checksum: 0x8d70b2b
#
# HEADLINE (Jun 2026, Xeon 8259CL / Cascade Lake): cc-x64 now reaches GCC
# PARITY on benchmark_core — median 1.07s vs gcc's 1.06s (was 1.39x at the
# start of this whole effort). The gap was code LAYOUT, not instruction
# selection, and it is now CLOSED in-compiler. Two effects, both gcc does:
#   (1) DSB µop-cache packing of the hot di->handler() loop  -> loop-head
#       alignment (S32_LOOP_ALIGN, default 32).
#   (2) Jcc erratum: a branch whose bytes cross a 32B line is evicted from the
#       DSB -> branch-straddle NOP padding (S32_BRANCH32, default on; gcc's
#       -mbranches-within-32B-boundaries analogue). x64_branch_pad() in
#       x64_encode.h, applied at jump/jcc/indirect-call choke points.
# Sweep (REPS=15, pad sweep = layout robustness):
#   baseline off               spread 20.4%  median 1.25
#   ALIGN=32 only              spread  2.7%  median 1.13
#   BRANCH32 + ALIGN=32 (NOW)  spread  4.7%  median 1.07  == gcc 1.06 (parity)
#   gcc reference              spread  1.0%  median 1.06
# Source-robust (fixes every straddling branch, not one loop's lucky phase);
# behavior-neutral (27 tests byte-identical, checksum 0x8d70b2b); +3% .text NOPs.
# Disable per-knob: S32_BRANCH32=0 / S32_LOOP_ALIGN=0.
# RULE THAT GOT US HERE: layout:run-noise ~25:1, so ALWAYS measure layout-area
# changes by the layout-sweep MEDIAN across offsets — a single-build A/B is
# worthless (it mis-killed loop-align once; verdict retracted, then this shipped).
# CORRECTION: an earlier note here said "not the Jcc erratum" — that was wrong,
# undercalled from too few branches/phases; the erratum was central.
# Durable codegen wins also landed: branchless boolean ternary (18a172fc) + a
# latent arg-marshalling miscompile fix. Heavier BOLT-style pass not needed.
```

**Bootstrap chain**: host GCC compiles `cc-x64.c` → `out/cc-x64` (the cross-compiler binary).
Then `cc-x64 --hir` compiles everything else (libc, emulator, tests).
`out/crt0.o` and `out/libc_x64.a` are pre-built; `make libc` rebuilds them.

**Architecture**: `cc-x64.c` `#include`s all headers — the compiler is a single translation unit.
Key files for codegen performance:

- `hir_regalloc_x64.h` — IRC graph-coloring register allocator
- `hir_codegen_x64.h` — x86-64 instruction emission, SIB/ADDI folds, compare-branch fusion
- `hir_burg_x64.h` — BURG instruction selection patterns
- `x64_encode.h` — raw x86-64 instruction encoding

**CRITICAL**: The `--hir` flag selects the HIR codegen path (with regalloc, SIB folds, LICM).
Without it, cc-x64 falls back to the tree-walk codegen which is 2x slower.
The Makefile passes `--hir` automatically.

## Testing Commands

```bash
# IMPORTANT: Run regression tests before committing any backend changes!
cd ~/slow-32/regression && ./run-tests.sh

# Run specific regression test
cd ~/slow-32/regression && ./run-tests.sh feature-arithmetic

# Cross-engine differential harness: every test under slow32, slow32-fast,
# slow32-dbt, and qemu, outputs diffed against the reference interpreter.
# Run after ANY emulator/DBT change.
cd ~/slow-32/regression && ./run-differential.sh

# Analyze binaries
./tools/utilities/slow32dump file.s32o    # Dump object file
./tools/utilities/slow32dump file.s32x    # Dump executable
./tools/utilities/slow32dis file.s32x [start] [end]  # Disassembler

# Performance test
time ./tools/emulator/slow32 program.s32x  # ~350M inst/sec
time ./tools/emulator/slow32-fast program.s32x  # Optimized version
```

## Regression Testing

**CRITICAL:** Before committing any LLVM backend changes:

1. Run the regression suite: `cd ~/slow-32/regression && ./run-tests.sh`
2. Check that no previously passing tests now fail
3. Document any intentional behavior changes
4. Add tests for new features or bug fixes

The regression suite helps catch unintended breakage. See `~/slow-32/regression/README.md` for details.

## Public mirror — ~/slow32-public, and what stays out of it

`~/slow32-public` (github.com/brazilofmux/slow32-public) is the copy the
outside world points at; it cannot be renamed. It is **not** a git
mirror: periodically a Claude Code session in that tree is asked to
pull commits over from `~/slow-32`, re-creating them by hand. Both
trees carry this file, so this section is addressed to that session
as much as to sessions here.

The private side of this tree is `~/majesty` (the user's own ledger)
and what `cobol/`, `clip/` and `dbase/` learned from it. The project
name and `~/majesty/...` paths are already public and fine. What must
not cross, in files **or commit messages**:

- ledger codes and the report file names built on them
  (`*-<code>.prn`); say "the activity report", "one balance sheet";
- account numbers, transaction ids, amounts and dates from the ledger
  or from majesty's test fixtures (`tests/cases/*`);
- vendor, payee and receipt names; family names other than the user's;
- anything under `cobol/out/` (gitignored here; it has held real data).

When a commit message here carries one of those, reword it on the way
over rather than skipping the commit. Known: `59c8fe53` (cobol Stage
16) quotes two fixture account numbers. `cobol/tests/data` is synthetic
(checked 2026-08-30 against the real files) and may cross as is.

## Issue Tracking — two channels, two numbering spaces

The project tracks work in two places on purpose. Know which one you are
writing to.

- **GitHub issues** — the cross-machine handoff inbox. An agent on one
  machine (Lenovo, kagura) hits a bug and files it out-of-band; whichever
  machine owns the fix picks it up and closes it. Use it for work another
  machine must act on, or when a durable external URL is wanted. That is
  how #5, #6 and #7 came to exist.
- **`ISSUES.md` (14 of them, per component) + `docs/IMPROVEMENTS.md`** —
  the in-tree engineering log: open items, and post-mortems of fixed ones
  kept next to the code they describe, versioned with it, greppable from
  any checkout. Deep debugging narratives belong here, not in a tracker —
  `eb49434c` was cracked using a lead recorded in `tools/dbt/ISSUES.md`.

Do **not** bulk-migrate the in-tree logs into GitHub issues. ~285 numbered
items across ~3,800 lines, many of them defensive code-review suggestions
rather than actionable bugs; filing them would destroy the tracker's signal.
Promote items one at a time, on the criteria above.

**CRITICAL — citing an in-tree item: never write a bare `#N`.**
The two numbering spaces overlap. Commit subjects already cite in-tree item
numbers across the range #1–#60 (48 commits, 36 distinct numbers, mostly
`selfhost/ISSUES.md`), while the GitHub counter is still climbing through
that same range from single digits. Consequences:

- GitHub resolves `#N` in a commit message at *render* time, so every new
  GitHub issue retroactively repoints an old in-tree reference at something
  unrelated.
- `fixes #N` / `closes #N` / `resolves #N` are GitHub auto-close keywords.
  A future commit saying `fixes #48` about a selfhost item will silently
  close GitHub issue #48 once that issue exists.

So: write in-tree items as **`selfhost ISSUES-48`**, **`DBT-14`**,
`runtime ISSUES-9` — component name plus a non-`#` number. Reserve bare
`#N` and the closing keywords exclusively for real GitHub issues.
(Historical commits using the old style are inert and stay as they are;
pushed history is not rewritten.)
