# f77 vs clang on SLOW-32 — LINPACK, 2026-08-27

Same kernel, same column-major indexing, same by-reference argument
passing, both targeting SLOW-32. The comparison measures **code
generation**, not ISA or algorithm (`linpack.f` and `linpack.c` are
deliberate mirrors of each other).

## Instructions (deterministic, the primary metric)

| compiler | instructions | ratio |
|---|---:|---:|
| clang -O2 | 501,352,191 | 1.00× |
| **f77** | 1,154,751,549 | **2.30×** |
| stage08 cc *(same C source)* | — | **3.56×** at 12 reps |

The third row is the important one. Compiling the **C** version with
`stage08 cc` gives 54.9M instructions against clang's 15.4M at the
smaller size, where f77 gives 50.2M. So **f77 is slightly better than
the project's own C compiler on this workload**, and the gap to clang
is a property of the shared SLOW-32 backend, not of the Fortran
frontend.

## Where the instructions go

| compiler | total insns | register moves | fp64 ops | moves per fp64 op |
|---|---:|---:|---:|---:|
| clang | 439 | 8 (1.8%) | 13 | **0.6** |
| f77 | 1258 | 207 (16.5%) | 9 | **23.0** |
| stage08 cc | 1099 | 214 (19.5%) | 9 | **23.8** |

Every double-precision operation is emitted as a call to a `__fp64_*`
helper, which the backend then inlines to a single hardware
instruction. But the *calling shape* survives inlining: operands are
forced into fixed `r4:r5` and `r6:r7`, and the result comes back in
`r1:r2`. That is ~23 register moves around one `fadd.d`. LLVM allocates
even-aligned pairs directly and emits `fadd.d rd, rs1, rs2` on whatever
registers the allocator chose — 0.6 moves per operation.

**Closed, partly — 2026-08-27.** The register allocator now claims
aligned pairs for fp64 halves and the emitter names them directly:

| | instructions | ratio | moves/fp64 op |
|---|---:|---:|---:|
| before | 1,668,542,079 | 3.33× | 23.0 |
| after | 1,154,751,549 | **2.30×** | **15.7** |

31% fewer instructions, 21% less wall time (0.2922 s → 0.2299 s).

Two parts. `hcg_pair_reg` lets the emitter name an already-aligned pair
instead of shuffling through `r4:r5`/`r6:r7` — safe on its own, since
it falls back to the moves. Then `ra_pair_claim` in `gc_select` claims
a free aligned pair for one half and *pins* the partner to the other.
The first attempt only looked for an already-coloured partner and
scored **zero hits**: select colours one node at a time, so the partner
is almost always still uncoloured (33 misses, 0 hits, measured). Claim-
and-pin scores 53 hits on this kernel. A pin is honoured only if it is
still conflict-free for the pinned node's own neighbours, so it can
never colour two interfering values the same; when it cannot be
honoured the emitter falls back to the moves.

The remaining 2.30× is inlining (clang inlines `daxpy` into its
callers) and the residual moves where a pair could not be claimed.

This work applies to `stage08` equally and is not yet ported there.

## The DBT is not the problem

Startup-corrected throughput (fixed overhead measured at 15.8 ms with
an empty program) is flat across workload character:

| workload | BIPS |
|---|---:|
| benchmark_core (integer) | 5.74 |
| f77 LINPACK (fp64) | 6.01 |
| clang LINPACK (fp64) | 5.88 |

So `slow32-dbt` executes fp64 code as fast per instruction as integer
code. The gap is visible *through* the DBT but is not *of* it — and
because throughput is flat, the 3.33× instruction ratio passes almost
1:1 into wall time (2.90×: 0.2934 s vs 0.1010 s).

**Caveat on BIPS:** it is a misleading metric here. f77 posts the
*highest* BIPS of the three precisely because it emits more trivial
`addi` moves, which translate cheaply. It still loses decisively on
time. Measure time, not BIPS.

**Caveat on short runs:** `benchmark_core` completes in 65 ms, of which
24% is process startup, so its raw BIPS (4.36) understates the DBT
badly. Any throughput claim needs the correction above.

## Reproduce

    ./run-bench.sh
