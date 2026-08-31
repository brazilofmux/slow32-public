# f77 vs clang on SLOW-32 — LINPACK, 2026-08-27

Same kernel, same column-major indexing, same by-reference argument
passing, both targeting SLOW-32. The comparison measures **code
generation**, not ISA or algorithm (`linpack.f` and `linpack.c` are
deliberate mirrors of each other).

## Instructions (deterministic, the primary metric)

| compiler | instructions | ratio |
|---|---:|---:|
| clang -O2 | 501,352,191 | 1.00× |
| **f77** | 994,174,607 | **1.98×** |
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

## Doubles now live in registers (fixed)

Double locals were **never** register-promoted: reaching a double's hi
word takes the alloca's address (`ADDI base,4`), and every promotion
scan treats an address-taken alloca as unpromotable. A routine with one
double and one integer emitted twelve frame accesses.

Fixed without touching the promoter, by removing the shape it cannot
handle rather than teaching it about pairs: a scalar double local now
gets **two allocas**, lo and hi, emitted together, with the hi slot
keeping the frame offset the `+4` would have addressed. Neither slot
ever has its address taken, so each has only direct word LOAD/STOREs —
exactly what `ssa_find_promo` accepts. The one-double loop went from 12
frame accesses to a body with none at all.

Taking such a double's address escapes **both** halves. The promoter
sees the lo alloca in `h_carg` and rejects it, but the hi alloca
appears nowhere and would have stayed promoted while a callee read
stale memory at offset+4. A dead `ADDI` on the hi slot is an
address-taking use the scan rejects, and DCE removes it afterwards, so
it costs nothing in the emitted code.

**On LINPACK this changes nothing** (2.144× → 2.145×): its doubles are
array elements and dummy arguments, not scalar locals. The win is in a
shape LINPACK does not have, so `bench/mandel.f` measures it — a
Mandelbrot iteration where every hot value is a double scalar:

| | instructions | ratio |
|---|---:|---:|
| clang -O2 | 23,399,759 | 1.00× |
| f77, splitting off | 186,807,480 | 7.98× |
| f77, splitting on | 126,669,044 | **5.41×** (−32%) |

Note what that says: the **scalar-double case is far worse than
LINPACK's 2.14×**, so it is where the remaining headroom is, not in the
array code. Disable with `F77_NO_SPLIT=1`.

### Spilling fp64 results directly (5.41× → 4.93×)

When an fp64 result is going to be spilled anyway, it is now stored
straight from the `r4:r5` the instruction produced it in, instead of
being moved through the `r1:r2` call-return convention first. Two fewer
instructions per spilled operation, and in fp64-heavy code most
temporaries do spill: the mandel hot loop went 62 → 52 instructions,
115.3M total. LINPACK unchanged.

### Where the last 4.93× lives: register pressure

The mandel hot loop is now 52 instructions against clang's 16, and
**26 of them are loads and stores** — spill traffic, not real work
(11 FP ops, 13 register moves, 26 memory).

The cause is structural. Every fp64 operation is an `HI_CALL`, so
`ra_mark_call_crossing` marks every value live across one as
call-crossing and bars it from the cheap `r3-r10` pool. In a loop with
eleven fp64 operations that is everything, leaving only the 18
callee-saved registers — and aligned-pair allocation fragments those
further.

Excluding fp64 calls from the crossing analysis was tried and is BOTH
wrong and slower (148.2M): `hcg_fp64_emit` really does clobber
`r4:r7` on its fallback path, so values allocated there would be
destroyed — the tests passed only by luck — and it lost 17% anyway.

### More registers does NOT fix it — measured, twice

The obvious fix is to stop fp64 ops being call boundaries, which needs
the scratch pair to come from registers that are neither allocatable
nor callee-saved. SLOW-32 reserves only `r1` and `r2`, which are not an
even-aligned pair, so that is a **register-convention change** — and
`CC_SLOW32` in the LLVM backend implements the same conventions, as
does the hand-written runtime assembly, with `run-interop-llvm.sh`
existing precisely to catch the two disagreeing. It would have to carry
through f77, stage08 and ~/llvm in lockstep.

**So the payoff was measured in f77 first, and it is not there.**

Two experiments, both bounding what such a change could buy:

1. *Precise clobber model.* Treat an inlined fp64 op as clobbering only
   its scratch (`r3-r7`) rather than the whole caller-saved pool,
   expressed as forbidden colours in `gc_select` — recovering `r8`,
   `r9`, `r10` for fp64-crossing values. Result: spill traffic fell
   (26 → 18 memory ops) but total instructions ROSE, 52 → 54. The
   freed registers bought less than the extra moves cost.

2. *Full exemption*, the ceiling of the ABI change — every value free
   to use all eight caller-saved registers across fp64 ops. Result:
   **17% worse** (126.7M → 148.2M). This version is also incorrect, and
   the whole test suite passed anyway; it was the performance number
   that exposed it.

Conclusion: **register count is not the binding constraint**, so the
ABI change would not pay for itself. What remains is the pair-alignment
handling itself — a question about the allocator, not the calling
convention. That turned out to be exactly right; see below.

## Pairs allocated as a unit (mandel 4.93× → 3.26×, LINPACK 2.14× → 1.98×)

LLVM models `f64` on this same ISA as a first-class register class:

    def Tuples2GPR : RegisterTuples<[gsub_0, gsub_1], ...>;
    def GPRPair : RegisterClass<"SLOW32", [f64], 64, (add Tuples2GPR)>;

so the allocator colours a double as ONE unit and alignment falls out.
Our allocator had two independent 32-bit values plus a preference. Two
changes closed most of the distance without a real pair class:

**1. Pairs are coloured before singles.** An aligned pair needs two
*adjacent* free colours, and singles scattered through the file destroy
those far faster than they consume capacity. Measured before: of 30
claims, 12 failed purely for want of a free aligned pair. Colouring
pairs in their own select pass halved that.

On its own this was a NET LOSS (4.93× → 5.18×) — and the reason is the
finding that mattered:

**2. Pairs share fate.** When a claim failed, the two halves were
coloured *independently*, so one landed in a register and the other
spilled — paying spill traffic AND the moves to rebuild the pair in
scratch. The hot loop's 26 memory operations had merely become 37
register moves. Now a pair that cannot be placed leaves BOTH halves in
memory, and the emitter loads them straight into its scratch pair.
"Allocated together, or spilled together" is precisely the property a
real pair register class provides for free.

Together:

| | mandel | hot loop | LINPACK |
|---|---:|---:|---:|
| start of this work | 7.98× | — | 3.33× |
| after pair *preference* | 5.41× | 62 insns, 26 mem | 2.30× |
| after direct spill | 4.93× | 52 insns, 26 mem | 2.14× |
| **pairs as a unit** | **3.26×** | **18 insns, 0 mem** | **1.98×** |
| clang | 1.00× | 16 insns, 2 mem | 1.00× |

The mandel hot loop is now 18 instructions against clang's 16, with no
spill traffic at all.

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

## Loop-carried phis coalesce across the call-crossing divide (1.98× → 1.81×, mandel 3.26× → 1.57×)

2026-08-30. Every counted loop was paying register copies per
iteration for its induction variable and trip count -- DAXPY's
increment block was four instructions where it should be two:

    addi r3, r22, 1        addi r22, r22, 1
    addi r4, r24, -1   →   addi r24, r24, -1
    addi r24, r4, 0
    addi r22, r3, 0

The phi-affinity moves existed and the dying-src1 exception was in
place; the blocker was `gc_coalesce`'s blanket refusal to coalesce a
move whose sides disagree on `ra_crosses_call`.  A loop-carried phi
ALWAYS crosses the body's fp64 pseudo-calls, and its increment
(defined after the last call, live only to the back edge) never does
-- so precisely the moves that matter most were all CONSTRAINED.

The refusal exists to keep non-crossing values eligible for cheap
caller-saved registers, which is right for ALU-affinity moves.  For a
PHI move it is backwards: the increment's value is about to be COPIED
into the phi's callee-saved register anyway, so coalescing costs
nothing and saves the copy.  Phi moves are now tagged at collection
and exempted from the mismatch refusal; the swap at the top of
gc_coalesce already puts the crossing node in `u`, so the merged node
keeps the callee-saved palette and correctness is unchanged.

| | LINPACK | mandel |
|---|---:|---:|
| before | 994,221,844 (1.98×) | 76.3M (3.26×) |
| after | 907,765,619 (**1.81×**) | 36.9M (**1.57×**) |

mandel halves because its hot loop is ALL loop-carried doubles: four
phi pairs, each paying two copies per iteration, all constrained by
the same rule.

Also kept from the same investigation: the dying-src1 interference
exception now covers HI_ADDI (opcode 40, outside the contiguous ALU
range the check tested) -- worth −1.6M on LINPACK alone.  Registering
ADDI *affinity moves*, by contrast, was measured at +26M and dropped:
flooding the move worklist with every address `+4` biases coloring
toward reusing base registers and loses more than it saves.

Two experiments that did NOT survive measurement, recorded per house
rule: scalar-dummy copy-in REVISITED with split lo/hi locals (the
2026-08-27 objection -- the copy was an unpromotable 8-byte alloca --
no longer applies).  The per-iteration win is real (DA's two loads
leave the loop), but keeping DA+N in callee-saved pairs for the whole
function raises pressure until the DY-hi address ADDI, live across
the fp64 pseudo-calls, spills INSIDE the loop: net zero per
iteration, plus copy-in/copy-out overhead per call.  1.983× → 2.001×.
Off by default, still `F77_COPYIN=1` to re-run.

Both engines and the oracle agree on every output; f77 suite 28/28.

## Blocks laid out by fallthrough chains (1.81× → 1.73×, mandel 1.57× → 1.52×)

2026-08-30. Emission order was CREATION order, and the frontend
creates a DO loop's exit block before the loop body exists -- so the
exit stub sat INSIDE the loop, and the body paid a taken jal over it
every iteration:

    .L46: body...            .L49: body...
          jal r0, .L48   →         (falls through)
    .L47: exit stub          .L51: increment
    .L48: increment          ...exit stub placed after the loop

hcg_compute_fwd now lays blocks out by greedy fallthrough chains:
each placed block is followed by its preferred successor (BR target;
BRC then-arm, the frontends' fallthrough arm) when unplaced,
otherwise the lowest-numbered unplaced block.  Block 0 stays first
(the prologue falls into it).  hcg_next_emit and hcg_blk_pos follow
the layout, so the existing fallthrough elision and bnear range
gating do the rest -- and the assembler's new branch relaxation makes
a long bcond safe rather than fatal, so layout changes cannot break
range correctness.

| | LINPACK | mandel |
|---|---:|---:|
| before | 907,765,619 (1.81×) | 36.9M (1.57×) |
| after | 867,099,154 (**1.73×**) | 35.7M (**1.52×**) |

Cumulative for the day: LINPACK 1.98× → 1.73×, mandel 3.26× → 1.52×.
f77 suite 28/28; LINPACK residual and mandel output verified.

## 1-based subscripts stop costing instructions (1.73× → 1.66×)

2026-08-30, two halves.  FRONTEND: f77_subscript_addr now accumulates
every constant lower bound × constant stride at COMPILE time and
emits it as one trailing ADDI on the final address -- so Fortran's
(I-1)*8 becomes I*8 with a -8 displacement, and multi-dimension
constant bounds fold into a single number.  Adjustable dimensions
keep the subtraction on the index, as they must.  BACKEND: BURG's
displacement fold already walked ADDI chains, but hcg_addi_folds_away
refused an ADDI whose user is another ADDI ("used as a value") -- the
exact shape a double's +4 hi-word address makes on top of the new
displacement ADDI -- and materialized dead address arithmetic.  The
predicate now recurses down the chain.

An hir_opt reassociation pass was tried first and REVERTED (+3.3M):
the in-place rewrite framework cannot insert nodes, so its ADD rule
needed the ADDI single-use -- and the (I-1)*8 value is shared by
both arrays in every copy loop, precisely where it mattered.  The
frontend owns the information; the frontend emits the shape.

DAXPY's body: addi/slli/add/add + 6 memory ops became slli/add/add +
6 memory ops with -8/-4 displacements.

| | LINPACK | mandel |
|---|---:|---:|
| before | 867,099,154 (1.73×) | 35.7M (1.52×) |
| after | 829,784,866 (**1.66×**) | 35.7M (1.52×, no subscripts in its loop) |

Cumulative for the day: LINPACK 1.98× → 1.66×, mandel 3.26× → 1.52×.

## Scalar dummy loads hoist out of loops (1.66× → 1.50×)

2026-08-30.  The copy-in experiment failed twice for the same reason:
a function-lifetime copy raises pressure.  The right scope is the
LOOP: F77 15.9.3.6 says a scalar dummy cannot legally be modified
through any other name while the subprogram executes, so a load of
one is loop-invariant even across the body's stores and calls -- the
aliasing knowledge is the FRONTEND's, and LICM has the machinery.

Mechanism: hi_emit grows a frontend-set flag, h_ld_ro; f77 sets it on
loads of scalar dummies (and of adjustable-dimension extents like
LDA, which are dummies too), and LICM treats a flagged LOAD with an
invariant address like a pure instruction.  One-pass compilation
means the disqualifying store may be seen AFTER the load, so flagged
loads are recorded per symbol and the flag is RETRACTED at unit end
for every dummy the unit stores: assignment, DO variable, READ
target, or its address passed onward to a callee.  Speculation is
safe -- the address is a dummy the caller already dereferenced.

DAXPY's DA now loads once in the preheader and lives in a
callee-saved pair; the body is 11 instructions:

    slli/add/ldw/ldw/add/ldw/ldw/fmul.d/fadd.d/stw/stw

| | LINPACK | mandel |
|---|---:|---:|
| before | 829,784,866 (1.66×) | 35.7M (1.52×) |
| after | 749,974,866 (**1.50×**) | 35.7M (no dummies in its loop) |

Cumulative for the day: LINPACK 1.98× → 1.50×, mandel 3.26× → 1.52×.

## Branch shapes unlocked; loop rotation tried and parked (→ 1.49×)

2026-08-30, closing the day.  Two emission-level gates were opened,
both enabled by the morning's assembler branch relaxation (GitHub
#22):

- `hcg_bnear` now always says near.  Its 2000-estimated-byte bound
  (≈330 real bytes, positions are over-estimated ~6×) predates the
  assembler relaxing long bconds; a direct bcond that ends up out of
  range is now rewritten by the assembler into exactly the fallback
  shape the gate used to force, so the direct shape is never worse.
- The direct conditional-branch shapes required PHI-FREE targets;
  they now require COPY-FREE EDGES (hcg_edge_nocopy): after
  coalescing, an integer loop's back-edge phi copies are no-ops, and
  refusing the direct shape cost a branch-over-jump per iteration.

LOOP ROTATION (bottom-test DO back edge) was implemented, measured,
and REVERTED: LINPACK 748.0M -> 707.4M (1.41×, DAXPY at its ideal 15
per iteration) but mandel 35.7M -> 61.2M (+71%).  The rotated back
edge carries the fp64 PAIR phis, and their copies do not coalesce
the way integer IV/trip phis do -- four register moves per iteration
per loop, plus knock-on allocation damage.  Rotation is a win
exactly when the back edge's phi copies coalesce to nothing; until
pair phis do, it stays out.  ctl_body[] remains recorded for the
retry.

End of day: LINPACK 748,228,640 (**1.49×**), mandel 35,684,791
(**1.52×**), from 1.98× and 3.26× this morning.  DAXPY's body is 11
instructions (was 15 plus 4 of loop overhead plus 2 copies).  All
28 tests green throughout; every step measured, three experiments
reverted on measurement (ADDI affinity, hir_opt reassociation,
rotation).

## Pair machinery made coalesce-safe; the rotation blocker re-diagnosed

2026-08-30, evening.  Went in expecting pair-phi coalescing to be the
rotation blocker.  It is not: measurements show pair-phi copies were
already coalescing after the morning's crossing exemption.  What the
investigation DID fix, and what it found:

- **Alias-blind pair machinery** (kept; measured neutral today,
  correct by construction): ra_pair_claim looked up partners through
  raw gc_node[] and pinned the un-aliased node -- after any coalesce
  involving a pair half, the pin landed on a node select never
  visits, and the pair fell back to scratch shuffles.  Pair identity
  now lives at NODE level (gc_pair_inst, propagated in gc_combine),
  partner lookup and pinning go through gc_get_alias, and the select
  pass filters and share-fate test key on the merged node.

- **Loop-depth-weighted spill cost** (kept; the right model): spill
  choice used STATIC use counts, so an innermost-loop phi looked as
  cheap to spill as an entry-block temp.  LICM now records
  licm_depth[] per block (each natural-loop body increments its
  blocks), and gc_select_spill weighs each use 1/10/100/1000 by the
  using block's depth.  Neutral on these two kernels, strictly
  better information.  F77_RA_DEBUG=1 dumps final spills.

- **The real rotation blocker, measured**: rotated and unrotated
  mandel both spill ~11 values -- but unrotated spills OUTER-loop
  phis (depth 1) while rotated spills INNER-loop phis and latch
  arithmetic (depth 3, the -140/-148 slots feeding the slow
  parallel-copy path).  The rotated form's innermost loop exceeds
  the 18-color callee pool by a node or two, and once any back-edge
  phi spills, hcg_phi_copies abandons its fast path for the whole
  edge.  Rotation needs LIVE-RANGE SPLITTING (spill outside the
  loop, reload at the preheader) or loop-aware coloring order --
  not better coalescing.  It stays parked; ctl_body[] still waits.

State: LINPACK 1.49×, mandel 1.52×, 28/28 tests.

## Spilled phis stop poisoning edges; rotation ships, gated (1.49× → 1.41×, mandel 1.52× → 1.50×)

2026-08-30, the splitting session's first installment.  Four changes,
each measured:

- **Spill-to-remat for pinned loop constants** (small): a wide
  constant pinned into a register by hcg_mark_loop_consts could
  SPILL, paying a frame slot while displacing values that needed the
  register.  gc_select_spill now treats pinned constants as the
  cheapest spill (cost 0), and writeback reverts an uncolored one to
  plain rematerialization -- no slot, no loads, register released.
  The rotated-mandel constant spills (6 of 12) vanished.

- **THE BIG ONE -- spilled phis no longer force the push/pop slow
  path** (rotated mandel 61.2M → 38.2M): hcg_phi_copies bailed to
  runtime-stack parallel copies if ANY phi on the edge was spilled.
  But a memory DESTINATION can never be part of a register cycle
  (emit it first, through r2), and a memory SOURCE is const-like
  (loaded straight into its destination register when it falls
  free).  Only a slot both read and written on the same edge, a far
  destination slot, or r2 as an endpoint still take the slow path.

- **Pair-friendly single placement** (small): the final first-free
  color scans now prefer a color whose aligned buddy is already
  used, so singles stop fragmenting virgin pairs.  Hot-pairs-first
  claiming was also tried and measured EXACTLY ZERO -- the failing
  claims fail because interference covers every aligned pair, not
  because of ordering -- and was reverted.

- **Rotation ships, gated on the frontend's pressure knowledge**: a
  loop whose body stores a scalar DOUBLE carries fp64 pair phis on
  its back edge, and rotating those loops loses (measured twice).
  f77_dstore_n counts scalar-double stores; close_do rotates only
  when the count is unchanged across the body and the DO variable is
  not itself a double.  DAXPY/DGEFA rotate; mandel's z-iteration does
  not.  The gate dissolves when live-range splitting lands.

| | LINPACK | mandel |
|---|---:|---:|
| before | 748,228,640 (1.49×) | 35,684,791 (1.52×) |
| after | 708,789,275 (**1.414×**) | 35,199,587 (**1.500×**) |

Both kernels at their best form simultaneously.  Day cumulative:
LINPACK 1.98× → 1.41×, mandel 3.26× → 1.50×.  28/28 tests; all
engines agree.  Still owed: live-range splitting (the two mandel pair
phis and two latch values that genuinely exceed the callee pool),
then ungating rotation, IV strength reduction, DGEFA shapes.

## Live-range splitting lands; the rotation gate dissolves (mandel 1.50× → 1.21×)

2026-08-30, the payoff.  A value LIVE THROUGH a loop but unused
inside it occupied a register for the whole loop under linear-
interval liveness -- exactly the register an inner-loop phi needed.
licm_split (running per natural loop, after hoisting, on the same
ssa_vis machinery) now stores such a value to a fresh frame slot in
the preheader and reloads it at the top of each post-loop use block,
rewriting the uses.  Candidates must be defined in a block
dominating the header, unused in the body, used only in blocks
dominated by the preheader; CALL/CALLHI results are excluded (their
pair linkage is positional).  The reloads live in split_head[b], a
new TOP-of-block list (LICM's lists run after a block's body, which
is right for hoists and wrong for reloads); ra_order, hcg_block, the
layout size estimate and the forwarding guard all learned about it.

Alone, splitting measured near-neutral (mandel −41K, LINPACK +0.6M
of store/load overshoot where pressure was already fine).  Its value
was never standalone: with registers freed across loops, the
scalar-double rotation gate came OFF -- and rotated mandel went
35.2M → **28.4M with ZERO register spills in the whole program**.
The chain that had to line up: phi coalescing across the crossing
divide, spilled-phi edge copies, spill-to-remat constants, weighted
spill costs, and splitting -- each necessary, only the ensemble
sufficient.

| | LINPACK | mandel |
|---|---:|---:|
| before | 708,789,275 (1.414×) | 35,199,587 (1.500×) |
| after | 707,773,312 (**1.412×**) | 28,421,688 (**1.211×**) |

Day cumulative: LINPACK 1.98× → 1.41×, mandel 3.26× → 1.21×.  28/28
tests; slow32, slow32-fast and slow32-dbt agree.  The gate machinery
(f77_dstore_n) is deleted, not parked -- splitting supersedes it.

## Derived induction variables (LINPACK 1.41× → 1.22×, mandel 1.21× → 1.16×)

2026-08-30, strength reduction as real SSA surgery.  licm_strred
(per natural loop, after hoisting and splitting): a header phi with
one back edge and an invariant step is a base IV; an in-body
SLL(iv,k) / MUL(iv,m) / ADD(iv,m) with invariant k/m becomes its own
phi -- init computed once in the preheader, its own increment in the
latch, uses rewritten, the original NOPped.  Two rounds, so
ADD(base, p) over a first-round derived p becomes a POINTER
induction variable.  ADDI candidates are excluded on purpose:
trailing displacement ADDIs fold into memory offsets, and deriving
them would unfold that.  Original IVs left with no use but their own
increment are swept, phi and all.

DAXPY's loop is now 13 instructions per iteration -- an 8-instruction
body (4 ldw, fmul.d, fadd.d, 2 stw, displacements folded) and a
5-instruction latch (trip decrement, TWO POINTER BUMPS, compare,
branch).  No shift, no address adds.  The clang loop this chases
carries 15 including a lui/addi global-address rebuild.

Three bugs found and fixed on the way, all recorded because each is
a class:
- LICM defers its global reference rewrite to the end of hir_licm
  but NOPs hoisted originals immediately -- so at strred time, users
  of a hoisted computation still name the original.  Deriving from
  the clone rewrote nothing; the deferred rewrite then pointed users
  at the NOPped clone.  Every operand comparison in strred now
  resolves one licm_map hop (sr_res).
- The same deferral hid the SLL's hoisted shift CONSTANT (kind
  checked as NOP, candidate silently skipped -- DAXPY got no SR at
  all until the operands were resolved too).
- split_frame seeded only from surviving ALLOCAs; in a function
  whose allocas were all promoted away, the first split slot landed
  on the frontend's reserved fp-4 -- the SAVED RETURN ADDRESS.
  SCALE stored J over r31's save and returned to J (slice9,
  latent since the splitting commit).  The seed now starts at 8.

| | LINPACK | mandel |
|---|---:|---:|
| before | 707,773,312 (1.412×) | 28,421,688 (1.211×) |
| after | 610,206,374 (**1.217×**) | 27,271,028 (**1.162×**) |

Day cumulative: LINPACK 1.98× → 1.22×, mandel 3.26× → 1.16×.  28/28
tests; all engines agree.  F77_SR_DEBUG=1 traces derivations.

## The gap investigated; one swapped operand worth 7% (1.22× → 1.13×)

2026-08-30, evening.  Three findings from actually profiling instead
of theorizing (bench/prof.py: parses the .s32x symbol table, streams
a slow32 -t trace, buckets instructions per function; run it on a
REPS=2 copy of the kernel):

1. **Inlining STILL loses, re-measured with today's optimizer**:
   F77_INLINE_MAX=12 gives 2.31×, =40 gives 1.224× (vs 1.217× off).
   The old verdict survives even though its old reason (dummy loads
   stay in memory) has expired -- the h_ld_ro hoisting works inside
   spliced bodies too.  Non-monotonic and never profitable; stays
   off.

2. **The profile**: DAXPY is 82% of f77's instructions.  And clang
   spends 13.6% of its total in MEMSET (its per-rep matrix re-init),
   so clang's pure compute is even tighter than the headline ratio
   -- worth remembering when reading these numbers.

3. **The real hot clang loop is 12/iter, not the 15 previously
   cited** (that was a cold dgesl loop rebuilding a lui/addi global
   address every iteration).  Clang's 12: one shared byte-offset IV,
   two address adds in-body, and a FUSED end-compare branch.  Our 13
   differed only in the trip test: slt + bne.

The fix: the compare-branch fuser handled SGT/SGTU/SLE/SLEU by
materializing the flag (slt r1; bne r1) -- two instructions --
when a > b is simply blt b,a with the operands swapped, and a <= b
is bge b,a.  One bcond now.  Every rotated loop's trip test
(SGT(t,0) -> blt r0,t) drops an instruction per iteration, and so
does every > or <= in an IF.

| | LINPACK | mandel |
|---|---:|---:|
| before | 610,206,374 (1.217×) | 27,271,028 (1.162×) |
| after | 566,760,085 (**1.130×**) | 26,120,168 (**1.113×**) |

DAXPY's loop is 12 per iteration -- clang's number.  Day cumulative:
LINPACK 1.98× → 1.13×, mandel 3.26× → 1.11×.  28/28 tests; residual
verified.  What remains vs clang on LINPACK is mostly DAXPY's call
overhead (prologue/epilogue × ~2000 calls/rep) and clang's memset
handicap partially offsetting it; the honest next levers are
save-fewer-registers shapes or revisiting inlining WITH a fix for
whatever makes it 2.3× at threshold 12.
