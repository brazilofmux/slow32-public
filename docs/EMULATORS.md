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

  | Host | benchmark_core @100M | Measured |
  |------|---------------:|----------|
  | Apple M5 Max | **7.50 BIPS** | 2026-07-16 |
  | Xeon 8259CL (EC2 m5.xlarge, virtualized) | **4.07 BIPS** | 2026-07-18 |

  Retired claims, for the record: **"~9.5 BIPS (x86-64)"** entered this file
  2026-07-02 (`c060f9c8`, the P2 doc-reconcile) with no in-repo measurement
  behind it. Its source is a Stage-5 profiling note — *"0.03s, ~9.5 BIPS"* —
  i.e. the checked-in 285M-instruction sprint (21% JIT warm-up) timed once on
  an unrecorded host. It does not reproduce on an actual Xeon (4.07, above).
  "~6 BIPS (Apple Silicon)" was likewise undated/unattributed. `TODO.md`'s
  3.5/4.6 BIPS are the same sprint arithmetic on another unstated config.
  **A BIPS figure without a machine, a build size, and a date is not a
  measurement — it's a rumor with units.**
  Best candidate for the unrecorded host, per author recollection (2026-07-18):
  a since-decommissioned Athlon x86-64, now in storage — *"it would have landed
  between the Xeon and the MacBook Pro. I think."* Untestable; recorded as a
  candidate, not a fact.

  **Do not read the x86-64 / AArch64 spread as attributing anything.** The two
  rows are different machines (Xeon vs Apple) running different translators, so
  the spread confounds hardware with translator and isolates neither. An earlier
  revision of this note blamed `translate_a64.c` for lacking `translate.c`'s
  loop pre-warm — **retracted**: the two translators use different register-cache
  designs, and each keeps loop bodies free of cold loads. `translate.c` is a
  lazy, demand-driven LRU, and the pre-warm exists to compensate for that
  laziness; `translate_a64.c` is a static prescan allocator that loads all eight
  slots once in the block prologue and takes back-edges as a bare `b.cond` — no
  flush, no reload (see the comment at `translate_a64.c:1304`). Neither owes the
  other's debts. The one clean same-host comparison available: on an M5 Max,
  `~/riscv`'s AArch64 DBT runs identical kernels 21% faster per guest
  instruction than this one (9.10 vs 7.50 BIPS) — cause unattributed,
  unprofiled. See `tools/dbt/ISSUES.md`.

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
  blocks translated vs 36), and the register-cache designs differ (adaptive LRU with
  warm-entry on riscv-a64 vs static prescan + eager prologue here — see above; both
  keep loops free of cold loads, neither is "incomplete"). The measurement was taken
  on a64. **The x86-64 run happened (2026-07-18, Cascade Lake Xeon), and it
  localizes the translator gap:** dbase reports came in at slow32-dbt 212.0 s vs
  rv32-run 177.7 s — ratio **1.19**, against the M5 Max's 1.38. Dividing out the
  instruction-count component (~1.14, travels with the binaries) leaves the
  per-guest-instruction translator ratio at **~1.04 on x64 vs ~1.21 on a64** —
  i.e. the two x64 backends are near parity and the "21%" is a fact about the
  two **a64** backends specifically. What survives on every host is the
  count component (compiler + fused branches, still not separable from each
  other). Caveats: both guests were rebuilt on the Xeon (updated riscv runtime;
  container LLVM for slow-32), and the count-ratio proxy comes from
  benchmark_core, not dbase. The twelve reports were byte-identical again.
  Profiling the two emitted a64 hot loops remains the open (optional) step.

  Real-workload replication (2026-07-17): the MAJESTY dBASE report suite — a
  54,481-record merge join, indexing, running balances, twelve reports — run
  end-to-end under both DBTs on the same M5 Max: slow32-dbt 55.5 s wall vs
  rv32-run 40.3 s. **Ratio 1.378 vs benchmark_core's 1.387** — two unrelated
  workloads agreeing within a point. All twelve report files byte-identical
  across the two guests. Same attribution caveats as above; the ratio is
  robust, the cause is still unassigned — but see above: the x86-64 rerun
  localizes the translator share to the a64 backends, and the rest is the
  compilers'.

  Separate flag from the same run: this Xeon executed the identical workload
  **~4x slower than the M5 Max** (177.7 s vs 40.3 s on rv; 212.0 vs 55.5 on
  s32) — while this file credits x86-64 with the DBT's best BIPS (~9.5).
  Both can be true (tight register loops are what a high-clock Xeon does
  best; a branchy indexed database is what Apple Silicon does best), but
  "both can be true" is exactly the 2-07 shape. benchmark_core on that Xeon
  at BENCH_ITERS=100M is the five-minute check. **Now done (2026-07-18), and it flipped the story.** On the same Xeon, same
  day: dbase has rv32-run ahead by 16% wall, but benchmark_core @100M has
  **slow32-dbt ahead by 14%** (0.70 s vs 0.81 s median-of-7; 4.07 vs 3.11
  BIPS; per-guest-instruction, slow32-dbt-x64 is ~31% faster on the kernels).
  So the per-instruction translator comparison is not a constant even per
  host — it is a function of (host, workload): rv +21% on M5 kernels,
  s32 +31% on Xeon kernels, near-parity on Xeon dbase. rv32-run's published
  "6.2 BIPS on a modern x86-64 host" also does not reproduce here (3.11) —
  the same rumor-with-units disease on the other side. Count-convention
  footnote: rv32-run's interpreter reports 2,518,751,041 for this build while
  its DBT stat reports 2,493,751,040 — Δ = 25,000,001 = mem_iters+1, i.e. one
  fused instruction per mem-loop iteration counted differently (~1%).
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
