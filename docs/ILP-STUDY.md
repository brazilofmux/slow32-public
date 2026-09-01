# How much parallelism is actually in a SLOW-32 program?

A dynamic ILP-limit study, run 2026-08-31, to answer one question before
any spatial/dataflow hardware effort starts: **if you threw unlimited
hardware at a SLOW-32 program, how many instructions per cycle could it
retire?**

The motivation is the FPGA arithmetic. `slow32-dbt` reaches ~9 BIPS on a
loop-heavy benchmark. At a 200 MHz fabric clock, matching that means 45
sustained instructions per cycle. Before spending a year in Vivado
finding out whether 45-way is reachable, it costs a weekend to ask
whether the *programs* contain 45-way parallelism at all.

They do not. Two of the three do not come close.

## Method

`tools/emulator/ilp.h`, hooked into the reference interpreter (`-I`).
Every retired instruction is recorded with its true register operands and,
for memory operations, its **exact effective address** — the analysis runs
inside the emulator, so there is no address reconstruction and no aliasing
guesswork.

Four models, all assuming **perfect branch prediction** (control
dependences are ignored entirely — that is what makes it a *limit*
study):

| # | model | what it measures |
|---|---|---|
| 1 | **dataflow** — unbounded width and window, memory perfectly disambiguated | the ceiling: pure register-dependence height |
| 2 | **+ true memory dependences** — a load waits for the last store to *its* address; stores respect WAW/WAR | the ceiling with real memory ordering, oracle addresses |
| 3 | **+ single memory port** — model 2, one memory op per cycle | SLOW-32's defining "single-ported memory" constraint |
| 4 | **in-order, W-wide** — in-order issue, W per cycle, true memory dependences | what a statically scheduled machine gets from *today's binaries* |

Models 1–3 are out-of-order with an infinite window; model 4 is in-order.
Real spatial designs land between them.

Each width in model 4 needs its own memory shadow, so the sweep runs one
width per process. (Sharing one shadow was a bug that made every width
report exactly 1.00 IPC: a narrow, slow width writes later timestamps,
which a wider one then inherits as bogus dependences.)

**Validation.** Two microbenchmarks with known answers, compiled by clang
at -O2:

| microbenchmark | expected | model 1 measured |
|---|---|---|
| one serial `x = x*3+1` chain | ~1 | **1.68** (chain + independent loop overhead) |
| eight independent such chains | ~8× the above | **8.68** |

Sample: 200M instructions from steady state (skipping 200M of startup;
LINPACK skips 50M). Memory-shadow evictions — the one source of
optimistic bias — stayed at or below 0.007% of memory operations on all
three workloads.

## The three workloads

Deliberately chosen to span the space, not to flatter the machine:

- **LINPACK** (`fortran/bench/linpack.c`, clang -O2): dense numeric, the
  friendly case, what a dataflow machine is supposed to eat.
- **DOOM** (`-timedemo demo3`): pointer chasing, branchy, the largest real
  C program the toolchain runs.
- **stage08 cc compiling s12cc.c**: the self-hosted compiler compiling its
  own 24k-line source. Recursive descent, hostile to everything.

## Results

Unit latency, 200M instructions each:

| | LINPACK | DOOM | compiler |
|---|---:|---:|---:|
| memory ops | 45.3% | 23.8% | 37.3% |
| **1. dataflow limit** | **7168** | **157** | **8.74** |
| 2. + true memory deps | 1824 | 104 | 8.74 |
| 3. + single memory port | 1.67 | 3.01 | 2.41 |
| single-port ceiling (1 / mem-fraction) | 2.21 | 4.20 | 2.68 |

Instruction mix:

| | load | store | branch | jump | alu | fp |
|---|---:|---:|---:|---:|---:|---:|
| LINPACK | 30.3% | 15.0% | 9.4% | 0.2% | 29.9% | 15.2% |
| DOOM | 17.2% | 6.6% | 10.7% | 1.2% | 63.9% | 0.0% |
| compiler | 22.4% | 14.9% | 7.6% | **14.2%** | 40.9% | 0.0% |

In-order, W-wide issue (model 4), IPC:

| W | LINPACK | DOOM | compiler |
|---:|---:|---:|---:|
| 1 | 1.00 | 1.00 | 1.00 |
| 2 | 1.53 | 1.61 | 1.63 |
| 4 | 2.01 | 2.00 | 2.24 |
| 8 | 2.02 | 2.08 | 2.28 |
| 16 | 2.02 | 2.09 | 2.28 |
| 32 | 2.02 | 2.09 | 2.29 |

With realistic latencies (load 2, mul 3, div 16, fp 3–12) models 1–2
barely move; model 3 degrades to 0.96 / 2.31 / 2.04.

## Three findings

**1. Intrinsic parallelism varies by 800× across workloads.** LINPACK has
7168 IPC available; the compiler has 8.74. There is no single fabric size
that serves both — hardware sized for LINPACK would sit at roughly 0.1%
utilization compiling its own toolchain. Note also that for the compiler,
models 1 and 2 are *identical to the cycle*: memory dependences add
nothing because register-dependence height already dominates. That
workload is not waiting on memory ordering, it is waiting on itself.

**2. The single memory port is the wall, and it is a low one.** Every
workload collapses to 1.7–3.0 IPC the moment memory operations serialize,
independent of how much parallelism the program contains. LINPACK loses a
factor of *1000* at that step. The ceiling is arithmetic: memory
operations are 24–45% of the instruction stream, so one port caps you at
2.2–4.2 IPC no matter how many ALUs you build. Forty-five ALUs against one
memory port is forty-two idle ALUs.

**3. Today's binaries deliver ~2 IPC to in-order hardware at any width.**
All three plateau at 2.0–2.3 IPC by W=4, and W=32 buys nothing over W=8
(LINPACK: 2.01 → 2.02). This is not a property of the programs — LINPACK
demonstrably contains 7168 — it is a property of the *instruction
schedule*. The code was scheduled by compilers targeting a scalar in-order
machine, and the microbenchmark shows the effect cleanly: eight
independent chains with 8.68 IPC of intrinsic parallelism deliver 1.53 to
an in-order machine of any width, because the chains are interleaved such
that each dependent pair forces a cycle advance.

## What this means for spatial hardware

The 45-way target is not reachable and would not be worth reaching. The
honest budget:

- **Memory must be banked first.** Nothing else matters until memory
  operations stop serializing. Six to twelve conflict-free banks buys the
  headroom; below that, extra ALUs are decoration. Note this means
  abandoning the property the machine is *named* for — a deliberate
  decision, not an implementation detail.
- **The compiler must reschedule, not just the hardware widen.** Width
  without a spatial scheduler is worth ~2 IPC. That is a compiler project,
  and the good news is that `hir_*` is already SSA at the right level.
- **Then the ceiling is workload-dependent and modest.** With banked
  memory and perfect scheduling, DOOM's ~100 and the compiler's ~8.7 bound
  what is achievable. A design targeting 8–16 way with 4–8 memory banks
  captures most of what exists in real programs; anything wider is
  provisioning for LINPACK alone.

Against the existing 5-stage scalar core in `fpga/rtl/` (~0.8 IPC after
branch flush and load-use stalls), a well-executed 8-wide spatial machine
with banked memory is plausibly a 3–10× improvement depending on workload
— not the 50× the DBT comparison implies. The DBT's 9 BIPS is not a
hardware target; it is a 5 GHz eight-wide out-of-order host with thirty
years of speculation machinery, measured on the friendliest possible
benchmark.

## Reproducing

```bash
cd tools/emulator && make slow32

# models 1-3 on any workload
S32_ILP_SKIP=200000000 S32_ILP_COUNT=200000000 \
    ./tools/emulator/slow32 -I -q program.s32x

# model 4: one width per run
S32_ILP_W=8 S32_ILP_SKIP=200000000 S32_ILP_COUNT=200000000 \
    ./tools/emulator/slow32 -I -q program.s32x

# realistic latencies instead of unit
S32_ILP_LAT=real ...
```

`-I` costs roughly 3× interpreter throughput and nothing when unset.
