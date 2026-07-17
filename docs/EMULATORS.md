# SLOW-32 Emulators

Five emulators exist with different trade-offs between capability, speed, and portability.

## Summary

| Emulator | Type | Speed | Full ISA | Full MMIO | Intrinsic Hooks | Source |
|----------|------|-------|----------|-----------|-----------------|--------|
| s32-emu | Interpreter | ~50 MIPS | Subset | Minimal | No | `selfhost/stage00/` |
| slow32 | Interpreter | ~240 MIPS | Yes | Yes | No | `tools/emulator/` |
| slow32-fast | Pre-decoded | ~240 MIPS | Yes | Yes | No | `tools/emulator/` |
| qemu-system-slow32 | TCG JIT | ~1 BIPS | Yes | Yes | Yes | `~/qemu` (separate repo) |
| slow32-dbt | DBT JIT | 7.5 BIPS (M5 Max) / ~6 BIPS (earlier Apple Silicon) / ~9.5 BIPS (x86-64) | Yes | Yes | Yes | `tools/dbt/` |

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
| AArch64 | Solid | Two wrong-code bugs (shifted-EOR fold, superblock back-edge) found by the differential harness and fixed Jul 2026 |

- **Build**: `make dbt` (gcc, auto-detects host architecture)
- **Run**: `./tools/dbt/slow32-dbt program.s32x`
- **Performance**: roughly 2.3 host instructions per guest instruction. BIPS is
  strongly host-dependent, so quote it *with the machine*:

  | Host | benchmark_core | Measured |
  |------|---------------:|----------|
  | x86-64 (Xeon 8259CL) | ~9.5 BIPS | — |
  | Apple M5 Max | **7.50 BIPS** | 2026-07-16 |
  | earlier Apple Silicon | ~6 BIPS | undated |

  **The x86-64 / AArch64 spread is probably not a hardware fact.**
  `translate.c` pre-warms loop-used registers into the cache at block entry, so
  the back-edge jumps past the cold loads and skips them every iteration
  (`loop_regs`, 16 sites). **`translate_a64.c` does not implement this at all.**
  On a loop-dominated benchmark that is worth real time, and it is the most
  likely explanation for a64 trailing x86-64 by ~25% on the same guest binary.
  Treat the AArch64 rows as measuring *the a64 translator's completeness*, not
  the silicon. See `tools/dbt/ISSUES.md`.

  M5 Max figure: `examples/benchmark_core.c` rebuilt at `BENCH_ITERS=100000000u`,
  2,850,025,393 instructions in 0.380 s (median of 5). **Do not use the checked-in
  `benchmark_core.s32x` for cross-project comparisons** — it is built at
  `BENCH_ITERS=10000000u` (285 M instructions), and at that size 21% of the runtime
  is JIT warm-up (`slow32-dbt -p`: translate 0.010 s of a 0.047 s run). `~/riscv`'s
  copy of the same source defaults to 100M, i.e. 10x larger; comparing the two
  checked-in binaries compares a sprint to a marathon. Rebuild both at the same
  `BENCH_ITERS` on the same host. Like-for-like on the M5 Max, `~/riscv`'s `rv32-run`
  does 2,493,751,040 instructions in 0.274 s (9.10 BIPS) — 39% faster in wall time,
  on 12.5% fewer instructions for identical kernels. **That gap is not attributed.**
  Three variables are uncontrolled: the compilers differ (gcc vs this tree's LLVM
  backend), the programs differ slightly (slow-32 links a libc for `printf`, 97
  blocks translated vs 36), and — see above — `translate_a64.c` is missing the loop
  pre-warm, while `~/riscv`'s `dbt_a64.c` has it. The measurement was taken on a64.
  Re-running both on x86-64, where this tree's translator is whole, would settle it.
- **Verification**: `--paranoid` lockstep shadow interpreter (verifies the
  register cache and in-block loops); `regression/run-differential.sh` diffs
  all engines on the full regression corpus

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
