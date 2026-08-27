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

## Folded address ADDIs suppressed (2.30× → 2.14×)

`x = base + 4`, emitted to reach the hi word of a double, is folded by
BURG into the following load/store's 12-bit displacement — leaving the
ADDI itself dead. DCE cannot see that: it runs long before BURG chooses
the fold. `hcg_addi_folds_away` suppresses the ADDI at emission, where
the fold is finally known, guarded by `users == bg_uses[idx]` so the
scan is proven to have seen every use (one ADDI commonly feeds both the
hi load and the hi store). 35 suppressed on this kernel; DAXPY's hot
loop went 19 → 17 instructions.

## The biggest remaining lever: doubles never live in registers

A program with one `DOUBLE PRECISION` variable and one integer emits
**twelve frame accesses**. Double locals are never register-promoted,
because reaching the hi word takes the alloca's address (`ADDI base,4`)
and every promotion scan treats an address-taken alloca as
unpromotable. So every double in every Fortran program lives in memory.

Fixing it needs **pair-aware mem2reg** — one alloca promoted to two SSA
values — which is a real change to `hir_ssa.h`. It is almost certainly
where the next large win is, and it would benefit `stage08` equally.

## Scalar dummy copy-in was tried, and lost

Fortran permits an optimisation C cannot: if a dummy argument is
assigned, no other name may be associated with the same storage
(F77 15.9.3.6), so a load of `DA` IS loop-invariant even though the
loop stores through `DY`. Copying scalar dummies into locals on entry
(and back at RETURN) should therefore hoist those loads.

It made things worse: **2.14× → 3.09×**, DAXPY's hot loop 17 → 30
instructions, the loop full of spills. Not because the reasoning is
wrong but because of the gap above — the "local copy" of a double is
still a memory access, so no load is saved, while the extra live values
push the allocator into spilling. Off by default; `F77_COPYIN=1` to
re-run the experiment.

## Inlining was tried, and lost

Implemented as source-level splicing (f77 is one-pass with no AST, so
the callee's body is re-lexed at the call site with its dummies bound
to the actuals' addresses). It is **off by default** because it is a
measured pessimisation at every threshold:

| inline cutoff | instructions | ratio |
|---|---:|---:|
| off | 1,154,751,490 | **2.30×** |
| 8 statements | 1,154,751,490 | 2.30× |
| 12 | 1,398,778,290 | 2.79× |
| 16 / 20 | 1,777,998,290 | 3.55× |
| 40 | 3,052,208,300 | 6.09× |

At the 12-statement setting it produced 17% more instructions and 26%
more load/store traffic (1401 vs 1196 static instructions, 474 vs 377
loads and stores).

**Why**, and it is specific to Fortran: arguments are BY REFERENCE, so
a dummy is an address whether or not the body is spliced. Inlining
`DAXPY` does not turn `DA` into a value — the inner loop still loads
through the address on every iteration. The splice buys only the call
and return, amortised over the callee's own loop and therefore nearly
nothing, while paying more live values and more spilling in a larger
function. C wins here because inlining lets the optimiser see `da` as a
value; Fortran would need **scalar replacement** of the dummy first,
which is an analysis this compiler does not have.

There is also structurally less on offer than in C: Fortran's tiny hot
operations are INTRINSICS (`DABS`, `DMAX1`), already emitted inline, so
what remains in user subprograms is loop bodies — the shape where
inlining pays least.

Kept behind `F77_INLINE_MAX=<statements>` so the experiment is
repeatable. `F77_NO_INLINE=1` forces it off.

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
