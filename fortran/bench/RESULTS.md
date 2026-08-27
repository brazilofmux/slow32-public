# f77 vs clang on SLOW-32 — LINPACK, 2026-08-27

Same kernel, same column-major indexing, same by-reference argument
passing, both targeting SLOW-32. The comparison measures **code
generation**, not ISA or algorithm (`linpack.f` and `linpack.c` are
deliberate mirrors of each other).

## Instructions (deterministic, the primary metric)

| compiler | instructions | ratio |
|---|---:|---:|
| clang -O2 | 501,352,191 | 1.00× |
| **f77** | 1,668,542,079 | **3.33×** |
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

Closing this means teaching the register allocator to allocate
even-aligned pairs for doubles. It would benefit `stage08` equally.

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
