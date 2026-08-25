# The Engine Room

Declared 2026-08-23, from the Redmond condo, the evening the ppu reel
froze the last graphics spec. The 1987 desk (1987-desk.md) is the
showroom: the apps, the glasses, the culture. This document is the
engine room: the machinery that lets the machine carry itself, fast,
on the hardware actually on the desk — Apple Silicon first, the x86
fleet second.

This deliberately **reopens the self-host performance front**. The
July ruling was "arrival declared, gcc parity, closed loop, done" —
and it was right, for the x86 cross-compilers on the machines of that
month. The desk moved to an M5 MacBook; the a64 side never got the
same love; and with all three tube modes clientele'd, this is the
push that's left. The old ruling is superseded by this document.

The front has three legs. They interlock: leg 1 makes the daily
driver honest, leg 2 is both a payoff and leg 1's best profiling
workload, leg 3 is the long game the other two feed.

## 1. slow32-dbt on arm64 — close the backend gap

The x64 DBT reaches ~9.5 BIPS (2.27 host cycles/guest inst,
chaining + superblocks + regcache — near-optimal, measured). The a64
port reaches ~6 BIPS. That gap was tolerable when arm64 was the
travel machine; it is now the primary dev box, so the a64 backend is
the fleet's slowest daily component.

First move is a profile, not a patch: split the ~35% into (a)
translation-quality gaps vs the x64 backend — which of chaining /
superblocks / regcache / addressing folds made it across the port,
and how well — and (b) genuine ISA/µarch differences that no backend
work recovers. Workloads: `benchmark_core` (the classic),
`forth/bench.fth` under the DTC kernel (dispatch-heavy — a stress the
core benchmark doesn't apply, and the workload class leg 2 will run),
and the DOOM timedemo (the show pony; also LinkedIn's favorite).

The standing dbt5 rule is not repealed: speculative stage-5-style
work stays binned until a workload demonstrates headroom. This leg is
about **parity with our own x64 backend**, which is proven territory,
not speculation.

**Profile taken 2026-08-23, the night the front was declared.** Three
hypotheses tested against the code and the clock:

- *Flags*: not it. RISC-V has no condition codes either — same
  materialized booleans (SLT ≈ SEQ), same fused reg-reg branches —
  and our a64 backend already uses host flags (guest `beq/bne` →
  `cmp + b.cond`, with cmp/branch fusion counters to prove it).
- *Memory checks*: not it. `-U` A/B on the M5: ~5% on the Forth
  bench, unmeasurable on benchmark_core — `bounds_elim` earns its
  keep. (Methodology note: the first A/B used a wrong flag spelling
  and produced a fantasy 15× — caught because the harness validates
  output. Always validate output.)
- ~~The register cache: it.~~ **RETRACTED, same night.** The a64
  backend HAS the regcache — under its own vocabulary
  (`reg_alloc`/`resolve_src`, 82 uses, 7 callee-saved slots, prescan,
  dirty tracking) — and it works: `-R` costs 40% on benchmark_core.
  The original claim came from grepping the other two codebases'
  names for it. Lesson re-learned: grep the file's own vocabulary
  before declaring absence.

**Corrected findings (2026-08-23, late), each measured:**

- **The a64 RAS was dead weight — removed.** `emit_ras_predict`
  popped the return-address stack, compared predicted vs actual, and
  *discarded the flags* — every return then fell into the generic
  probe anyway (the function's own comments talk themselves out of
  using the prediction, but the emission stayed). ~10 dead insts per
  return + ~7 per call. Forth EXIT block: 104 → 68 host bytes.
  Wall-clock effect on the M5: ~2% on the Forth bench — the wide
  core was absorbing most of the waste. Disabled arch-scoped in
  dbt.c + translate_a64.c; x64's real RAS untouched. Full battery
  green (regression 77, differential 73/4-allowlisted, forth 26,
  reel goldens, doom timedemo hash).
- **The rv32 sibling's edge, decomposed** (identical bench, guest
  inst counts within 1%): fib-only (dispatch/call/return dense)
  1.43×; nested loops 1.19×; whole Forth bench 1.34×. Not checks
  (`-U` flat), not superblocks (`-S` slightly worse off), not the
  regcache (`-R` flat on Forth — per-block caching can't help
  4-inst blocks). The gap is concentrated in the emitted
  dispatch-path code itself.
- **Next probe, for a fresh sitting:** side-by-side host code of
  NEXT/DOCOL/(LOOP) blocks, ours vs rv32's, instruction-by-
  instruction on the serial dependency chain (the indirect-probe
  path is ~4 dependent loads deep); count host insts/dispatch on
  both. The rv32 backend's lazy rc_read (no prologue loads) and
  self-loop warm-entry are the visible structural differences left.

**The probe ran (2026-08-23, same night). Leg 1 closes.** Done with
`S32_DBT_DUMP_PC` (new: dump any translated block by guest PC — the
`-d` counter only sees dispatcher entries, so chained blocks were
invisible) and `/usr/bin/time -l`'s retired-instruction counters:

- **Instruction-density parity with x64 is already real.** On
  benchmark_core the a64 backend emits 2.24 host instructions per
  guest instruction — the x64 backend's storied number is 2.27. At
  0.67 host *cycles* per guest instruction on the M5, the "~6 vs
  ~9.5 BIPS" framing this leg was declared under turns out to be a
  cross-machine clock/µarch artifact, not a backend gap.
- **The Forth-dispatch gap is real but latency-bound, and the M5
  proves it:** `-U` cuts 33% of retired instructions and saves zero
  cycles — IPC falls 7.9 → 5.1 as the spare-slot padding disappears.
  Our NEXT block runs 12.3 host inst/guest inst vs rv32's ~7.5
  (bounds checks, branch-over-fault, write-only prologue loads,
  scratch-mov churn), and the M5 hides essentially all of it. The
  residual 1.4× vs the rv32 sibling works out to ~2 cycles per
  dispatch of critical-path difference — microarchitectural dust
  (both DBTs even share the same block structure: rv32 also ends
  blocks at plain `jal`). Chasing it is not worth a sitting.
- **What this proves about leg 2:** dispatch cost on this machine is
  *serial* — no amount of DBT-side instruction shaving removes it.
  The 4× Forth win lives exactly where the plan put it: forthc
  deleting the dispatch loop itself. Leg 1's measurement is leg 2's
  justification.
- Instruction-shaving items worth keeping for the *x64 fleet*
  (narrow cores pay for width where the M5 doesn't): fault stubs
  out-of-line via the existing deferred-exit machinery, write-only
  prologue-load elision, loading guest memory straight into cache
  registers. File under "when an x86 box feels slow," not now.

Leg 1 verdict: dead RAS removed (877690a9), density parity
confirmed, dispatch residue quantified at ~2 cycles/dispatch and
deliberately left. The leg's remaining value is monitoring, not
work. **Proceed to leg 2.**

**Scoreboard rerun (end of the same day, M5 Max, benchmark_core
@100M, medians of 5):** slow32-dbt 8.26 BIPS (July's row: 7.50 —
+10% from this leg's work), rv32-run 8.72 BIPS (July: 9.10). The
21%-"cause unknown" per-guest-instruction gap ~/riscv recorded on
2026-07-16 is now 6% and attributed; roughly two-thirds of the
remainder is the dormant select fusion (see the contract follow-up
below). Canonical number home: docs/EMULATORS.md, dated rows only.

**Coda (same night): the guest-codegen layer, measured.** The user's
follow-up hypothesis — GCC generates easier-to-DBT rv32 code than
our LLVM backend generates SLOW-32, so the DBT's hands are tied
upstream — tested on identical benchmark_core kernels through both
stacks, normalized per iteration:

| per iteration | rv32 (GCC) | slow32 (our LLVM) |
|---|---|---|
| guest instructions | 24.9 | 28.5 (+14%) |
| host inst / guest inst | 2.33 | 2.17 |
| cycles / iteration | 12.6 | 15.3 (+21%) |

DBT layer: parity (ours slightly denser). The 21% end-to-end gap is
**compiler-layer**, and the hot-loop diff names the mechanism: for
`if ((acc & 0x10) == 0) acc = acc << 1 | 1;` GCC emits a 3-inst
branchy form (`andi; bnez; ori`) while our LLVM emits an 8-inst
branchless mask-select whose 5-deep dependent chain rides the
loop-carried `acc` — serial latency the OoO host cannot hide, spent
to avoid a branch the host predicts anyway. (cc-x64's branchless
ternary win, 18a172fc, is the same idiom pointing the other way —
the right answer is host-dependent.)

**Where the fix lives:** not the LLVM backend — it stays frozen as
leg 3's oracle. This finding transfers to **leg 3's quality bar**:
stage08's slow32 codegen now has a measured external reference
(GCC-for-rv32 at 24.9 inst/iter on this kernel set), and
if-conversion/select policy is the first named lever. "Sufficiency"
for leg 3 quietly gained a number to chase.

**The homework (same night): which shape should the guest canon
be?** The user's reframe: the guest ISA is an IR between two
compilers we own; if the two DBT hosts prefer different code
flavors, the compiler should emit whichever shape the DBT can most
cheaply transform, and the DBT re-flavors per host. Answered:

- Branchy → host-select needs if-conversion inside the DBT
  (diamond recognition, speculation-safety proofs, block-boundary
  surgery — branches end blocks in both our DBTs). Hard.
- Branchless mask-select → host-select is an adjacency peephole
  over pure dataflow — no control flow touched, no proofs needed —
  and the machinery half-exists (`pending_cond` compare-deferral in
  translate_a64.c is the same shape).
- Our LLVM backend already emits the branchless form as ONE
  canonical idiom (`LowerSELECT`: `F ^ ((T ^ F) & -C)`, with a
  branchy `SELECT_PSEUDO` already in-tree for -Os). So the canon is
  already right; the DBT just translates it literally instead of
  recognizing it.

**Work item (leg 1 reopens for exactly this): select-idiom fusion in
both DBT backends** — recognize the canonical mask-select, emit
`cmp+csel` (a64) / `cmp+cmov` (x64). Expected: collapses 5-8 guest
ALU ops with a 5-deep loop-carried chain to 2 host insts, 2-deep —
strictly better than BOTH literal flavors on BOTH hosts, no
misprediction exposure. Measured target: close most of the 21%
cycles/iter gap vs GCC-rv32 on bench_branch-class code.

**Doctrine (name it once): the idiom contract.** The backend's
canonical lowering shapes are documented, stable, recognizable
idioms; each DBT backend peepholes them to its host's optimum. The
compiler emits transformable shapes, not host-flavored ones. First
entry: the mask-select. Candidates to audit later through the same
lens: min/max, abs, byte-swap, carry chains.

**Shipped (2026-08-23, the same sitting).** Both sides of the
contract's first entry:

- *Compiler side*: one td pattern — negation anchors on the
  architectural zero (`sub rX, r0, rC`), never a CSE'd zero in an
  allocatable register, which had made the mask unprovable across
  blocks. Semantics identical; this is canonicalization, not the
  frozen-oracle drift the leg-3 rule guards against.
- *DBT side (a64)*: `select_idiom_scan` in the prescan recognizes
  the five-op mask-select by use-def walk (operand order agnostic,
  interleaved T-computation tolerated, r0 anchor required for
  soundness); the blend-final XOR emits `CMP + CSEL` instead. Only
  the final XOR is replaced, so a mismatched pattern can never be
  unsound — the negative control (allocated-register zero) refuses
  by construction. Kill switch: `S32_DBT_NO_SELECT_FUSE=1`.
- *Measured* (benchmark_core, median of 7, checksum intact):
  166.9M → 151.7M host cycles, **−9.1% whole-benchmark** from the
  one idiom; per-iteration 16.0 → 14.5 vs GCC-rv32's 12.6 — the
  gap shrinks from +21% to +15%, the remainder being general
  instruction-count surplus in the other kernels, not selects.
- x64 DBT translates the new canon literally (correct, unfused);
  its cmp+cmov fusion is queued for an x86 sitting.

**The kernel audit finished the same night, and found a bigger fish
than any loop.** Per-iteration scorecard vs GCC-rv32: bench_branch
fixed by the fusion; bench_mem at parity (12 vs 12, different
routes); bench_arith 8 vs 7 — GCC does linear-function test
replacement (the seed accumulator doubles as the exit test) where we
carry a separate down-counter. Small fish, noted. The big fish:
**every function was paying a full lr/fp frame — including pure
leaves with zero stack use** — because nobody had told the clang
driver that SLOW-32 may omit frame pointers when optimizing. One
line in `useFramePointerForTargetByDefault` (riscv-style: frames
only at -O0), and:

- tiny leaf: 10 instructions → 3
- DOOM: −2.1% guest instructions (4.640G → 4.542G), −2.9% text,
  −5.1% host instructions and −3.6% host cycles under the DBT,
  and **all 2173 timedemo frame hashes bit-identical** — the
  determinism proof now spans codegen shapes, not just engines.
- Measurement trap logged: DOOM's timedemo wall time is
  ~85% sleep (frame pacing), so wall A/Bs are noise — an apparent
  +7% "regression" inverted to −3.6% under CPU counters. Cycles
  retired, not wall clock, for paced workloads.

Compiler-side changes live as one local llvm-project commit
(e507704cf3c4), mirrored: td via backup.sh, CommonArgs via the
integration patches (generate-patches.sh now includes it).

**x64 select fusion shipped (2026-08-23, the Intel sitting) — and the
frame-pointer rebuild quietly moved the goalposts.** The queued
cmp+cmov port landed in translate.c (same prescan scan, same
kill switch, CMOVcc encoder added to emit_x64.h; fusions surface in
`-s` as "Select-idiom fusions"), but pointing it at the CURRENT
benchmark_core — rebuilt after the frame-pointer change — found
**zero matchable canonical selects**. The fp rebuild shifted register
allocation, and the hot loop now reuses one register three ways
(`andi r7,…; seq r7,r7,r1; sub r7,zero,r7`): the compare operand is
clobbered by the mask, so both backends' shipped scans refuse. The
a64's measured −9.1% predates these bits — **re-measure on the Mac;
it is likely gone.**

Two DBT-side answers landed on x64:

- *Operand rematerialization*: a compare operand whose register was
  reused is recomputed at the fusion point when its def is a simple
  reg-op-imm (ANDI/ADDI/ORI/XORI/shift-imm) whose source survives —
  guarded against in-block back-edge re-entry between def and blend.
- *T-recovery*: the hot loop also computes `t2 = T ^ F` **in place
  over T's register**, which exposed a latent soundness hole in the
  shipped a64 scan — the destroying def is instruction x itself, so
  `defined_between(x, k, T)` cannot see it, and a fused select reads
  t2 where T was meant. Never triggered on a64 only because that loop
  was rejected earlier on the clobbered compare operand. x64 recovers
  T = t2 ^ F (both live at the blend, exactly the literal values);
  translate_a64.c got the conservative reject (`T == t2 || f == t2`)
  — untested on ARM hardware, verify on the Mac.

Measured (Cascade Lake VM, task-clock median of 21, checksum
0x8d70b2b intact): benchmark_core 71.1 ms → 71.1 ms — **parity, not
a win**. The remat+recovery shape only shortens the loop-carried
chain by ~1 level (the literal intermediates still execute; x64 has
no dead-temp elimination), and the recovered operands cost as much
as they save. A directed microbench of the *canonical* shape with
surviving operands (the shape the contract promises) shows −2.2% —
the mechanism works; the guest code stopped holding up its end.

**Contract follow-up (Macbook, compiler-side):** the fp rebuild made
the canon untransformable — regalloc reuses the setcc operand's and
T's registers because they die inside the idiom. Either the lowering
keeps them live through the blend (transformability as a stated
canon property), or the DBT-side remat/recovery is accepted as the
contract's other half and ported to a64. Decide with the a64
re-measurement in hand.

Battery for the x64 sitting: hand-asm suite (now committed:
regression/tests/feature-dbt-select-fuse — 8 positive shapes fuse
including remat and in-place-t2, allocated-zero and FMT_R-clobber
controls refuse), regression 78/78 effective (76 in the toolchain
container + the two tube tests verified on the host: the stale image
lacks python3/s32-crt — run-tests-docker.sh now builds assembler and
linker from workspace source so image staleness stops failing them),
differential 78/78 agree, forth 26/26 on dbt, ppu-reel PASS with
golden hashes.

## 2. forthc — the AOT Forth compiler

Elevated from the curiosity shelf (1987-desk.md §8) to an engine-room
leg: it stops being "is our Forth fast enough" (it is) and becomes
the front's best probe — a compiler whose output is dispatch-free
guest code, measured through the DBT leg 1 is improving.

Design is settled and on record: W^X is not negotiable, so this is
AOT only — `forthc` compiles `.fth` to `.s32o` (primitives inlined,
the rest subroutine-threaded), linked by the selfhost `s32-ld`;
turnkey `.s32x` images via `exec` are the interactive compromise. The
object-emission machinery already exists in Forth (stage01's
`asm.fth`/`link.fth`). The DTC kernel stays forever: bootstrap seed
and REPL.

Baseline (2026-08-23, M5 Max, startup-corrected, heavy bench):
gforth-fast 66 ms, gforth 83 ms, DTC-under-dbt 260 ms, DTC-under-fast
4.19 s. Win condition: compiled bench ≥2× faster than DTC bench under
the same DBT, same machine. Stretch: parity with gforth-fast on the
Mac — noting that ratio is partly environmental (leg 1's gap; gforth
0.7.3 vintage), so the honest scoreboard is our own before/after.

## 3. stage08 takes over from LLVM

The long game: the day the desk's applications build with the
compiler the desk built. stage08 already holds gcc parity on the x64
cross, a working a64 cross, and a pinned LLVM interop proof
(db3b37a4). What it is not yet is the default way a shipped app gets
built for SLOW-32.

Scope, stated plainly (ruled 2026-08-23): stage08 is **not
equivalent to LLVM and never will be** — not in optimization
breadth, not in language completeness, not in targets. The target is
sufficiency: the C that the desk's programs actually write, compiled
correctly and fast enough that nobody reaches for clang out of
habit. "For our purposes, probably" is the whole ambition, and the
milestones below are how "probably" gets replaced with a checklist.

Milestones, each a gate, none a promise:

1. **One shipped app.** rogue or sbasic (real C, real test suites)
   builds with stage08 targeting slow32 and passes its suite.
   **LANDED 2026-08-24 (bef81eed): rogue, all five TUs compiled by
   stage08 cc, passes all 23 checks** — winner run, save/restore,
   10-seed soak (`rogue/tests/run-tests-s08.sh`). Language gaps
   closed en route: sizeof(expr) in constant contexts, and 2D
   arrays (flat layout + recorded column count; first subscript
   scales by a row and stays an address; >2D and 2D initializers
   are honest errors). The gate also flushed out an LLVM oracle
   bug (SPAdj disarmed by canSimplifyCallFramePseudos=true —
   frameless locals in call regions resolved low; fixed and
   validated against rogue/regression/DOOM goldens) plus the
   extern-.bss fix and s32-as branch range checks. sbasic waits on
   HW FP.
2. **The tube demos and the reel.** Frame hashes are
   compiler-independent truths — a stage08-built reel must produce
   the same 14 golden hashes. First cross-compiler conformance test
   that costs nothing to run.
3. **The ABI divergences stop being footnotes.** Double-argument
   passing and struct-by-value are "known-divergent, not gated" in
   the interop proof. Taking over means picking conventions and
   closing them.
   **LANDED 2026-08-24 (3f027ad4), completing the same-day sweep:
   doubles now use clang's aligned-pair convention** (hi_abi_assign
   is the one shared ABI walk for calls, params, and codegen entry;
   the interop double probe matches), **and sbasic — 13 TUs of
   doubles and structs-by-value — builds under stage08 cc
   output-identical to the clang build on all 45 suite tests.**
   The forcing app flushed out two deep codegen bugs (the
   large-frame epilogue used r1 as an address scratch and clobbered
   return values; tail calls popped frames whose locals' addresses
   had escaped as arguments), a param-emission ordering hazard
   (copy loops stole later params' incoming registers before their
   PARAM defs existed), the struct-return shift leaving ABI tags
   behind, pair-blind ternaries, a silently-dropped double compound
   assignment (every BASIC FOR loop spun), and pointer-to-array
   members. Both fell to the emulator's `-w` watchpoint once printf
   bisection had named the frame.
   **Struct-by-value ARG convention CLOSED 2026-08-24: stage08 now
   speaks clang's byval** — the caller copies the struct into its own
   frame and passes the copy's address in a stack slot reserving the
   struct's full rounded size (never an argument register; stack
   slots laid out in argument order), so no interop divergence
   remains.  run-interop-llvm.sh now GATES doubles, struct args,
   struct return (shared sret-in-r3), the byval slot layout after
   8 register args, and caller-copy semantics (clang callee mutates
   its copy; stage08 caller's local must survive) — both directions.
   Full gate ladder re-run green under the new ABI: fixed-point
   56/56, sbasic 45/45 output-identical, rogue 23/23 + soak,
   graphics reel/vecscope/fire/sprites frame-identical, DOOM 2173
   frames bit-exact on both engines, benchmark_core checksum exact.
   **HW FP EMISSION CLOSED 2026-08-24 — leg 3's formal items are
   complete.** stage08 cc emits the real SLOW-32 FP instructions:
   codegen recognises the synthesized __fp64_* pair libcalls and
   inlines the fadd.d/fcvt.* family (even-aligned scratch pairs
   r4:r5/r6:r7; call clobber semantics kept, so the allocator needed
   no pair support — args are call-crossing, never r3-r10).  f32 was
   already native.  The rabbit hole underneath, all fixed: double
   LITERALS were parsed at f32 precision (lexer's 24-bit converter —
   now native-double conversion with an exact-5^e compensated
   Dekker ladder + exact 2^e scale, correctly rounded to the last
   bit outside the denormal band); global FP initializers (scalars
   with int values, and every array element) truncated through
   parse_const_int (now ps_fp_init_store_at, 8-byte IEEE emission);
   and BOTH stage07 and stage08 allocated 4-byte slots for zero-init
   double scalar globals, so 8-byte stores stomped the next global
   (stage07 repaired per ruling — a better stage07, not stage08;
   its cc.s32x rebuilt via stage06, sums updated).  Discovered en
   route and documented: stage07 cannot pass/return doubles (or
   long longs) as function values — the lexer's twoProd communicates
   through statics for that reason.  New standing gate:
   run-fp-differential.sh — 288 FP results (arith/compare/convert,
   1e±300 extremes, global-array inits) bit-identical between a
   stage08 build and the LLVM build, plus a no-__fp64-calls check.
   Transcendentals stay libcalls the DBT hot-swaps, per the
   platform FP architecture.  Full ladder green under HW FP:
   stage07 53/53, fixed-point 56/56, interop, sbasic 45/45 (eval.s
   has ZERO __fp64 calls now), rogue, graphics, DOOM 2173 bit-exact
   both engines, benchmark_core exact, purity clean.
   **POLISH SWEEP (day after):** the leftover minor items all fell,
   and pulling on them found real bugs.  (1) The "sizeof +4"
   divergence was struct ALIGNMENT: stage08 aligned double/llong to
   8 where clang's SLOW-32 ABI uses 4 — member offsets diverged too
   (offsetof a post-double field was +4 off), so any mixed-object
   struct with a 64-bit member was layout-incompatible.  ty_align
   now returns 4 on the slow32 target (cross targets keep native 8);
   interop gains a struct-layout gate (char/double/int by pointer).
   (2) cc-a64's cc_dbt_c_forms test was INVALID C (assigning to a 2D
   array row — host cc rejects it too); the "not an lvalue" error
   was stage08-lineage 2D semantics working correctly.  Test fixed;
   that un-dammed the rest of the long-blocked a64 suite, exposing:
   (3) ssa_find_promo consulted ssa_promo[] BEFORE rebuilding it, so
   an escaping alloca whose inst id collided with the PREVIOUS
   function's stale promo entry skipped rejection and was wrongly
   promoted (cc-a64: a double literal's temp alloca escaped into
   ADDI+4, got promoted, and the f64 load collapsed to its lo-word
   ICONST — one_double(0, 3.75) received 0.0).  Fixed in shared src
   AND repaired in place in stage05/06/07's frozen copies (ruling:
   better stageN, never stageN+1; only the three cc.s32x binaries
   changed — every rebuilt tool hashed byte-identical).  (4) a64
   HI_VA_ARG clobbered its own control-block base: a SPILLED result
   materialises with dst == HX_SCRATCH1 (the base register), and the
   value load preceded the pointer write-back — snprintf("%x", 255)
   stored through the VALUE and segfaulted (%d worked only because
   its result drew a real register).  Both paths now finish every
   base use before the final value load (x64's emitter always did).
   (5) t_printf_f was an orphaned ad-hoc binary, never in the suite;
   %f/%g/%e verified working under cc-a64 today; cruft removed.
   RESULT: the a64 suite runs END-TO-END GREEN for the first time
   (all cc_* tests + 34/34 diff-tests, zero mismatches); full slow32
   ladder re-verified green (stage06 47/47, stage07 53/53, stage08
   56/56 fixed-point, interop incl. new layout gate, FP differential
   288/288, sbasic 45/45, rogue, graphics, DOOM 2173 bit-exact both
   engines, benchmark checksum, purity).
4. **DOOM.** Built by stage08, `-timedemo demo3` runs to completion —
   and because the game logic is fixed-point-deterministic, the 2173
   frame hashes should match the clang build's goldens exactly. Same
   answer from a different compiler: the final boss, again.
   **LANDED 2026-08-24, the same day as milestones 1 and 2
   (b2911110): all 84 TUs compiled by stage08 cc, all 2173 frames
   hash-identical to the clang goldens on BOTH slow32-fast and
   slow32-dbt** (`scripts/test-stage08-doom.sh`). The bug ladder it
   flushed out — a preprocessor define that silently dropped
   `*FRACUNIT` after a leading number (weapon never reached the
   top; gameplay diverged at frame 16), the 4-byte struct-size
   floor breaking on-disk WAD strides, unscaled pointer
   subtraction, unsigned div/rem emitted as signed instructions,
   three regalloc operand-reuse paths leaking caller-saved colors
   to call-crossing values, array typedefs decaying at
   registration, named init-relocs all emitting gname[0] — is
   recorded in that commit. Method: clang/stage08 objects
   interoperate, so every bug fell to mixed-object bisection, then
   per-function hybrid assembly, then deterministic frame-hash
   comparison. (Milestone 3's then-open items — struct-by-value
   passing and double args — have since closed; HW FP emission is
   the one left.)

LLVM is not deleted at the end of this. It becomes the reference —
the differential oracle stage08 is measured against, the way gforth
oracles stage01. "Taking over" means default, not exile.

**Leg 3 opened 2026-08-24 — the survey (the leg's number).**
benchmark_core through stage08 cc (`cc.s32x` under the DBT):
- Ergonomics: already solved — 31 ms to compile under slow32-dbt;
  cc/s32-as/s32-ar/s32-ld all exist as .s32x and run hosted.
- Correctness: exact reference checksum (0x8d70b2b) first try; the
  verification estate (fixed-point gate, diff-corpus, interop, LLVM
  compare) already exists.
- **The mountain: 63.1 guest inst/iter vs LLVM 28.5 vs GCC-rv32
  24.9 (2.2×; 3.6× in DBT cycles — 561M vs 154M — because the waste
  sits on loop-carried chains the M5 cannot absorb).** Ten lines of
  bench_arith name the defects: branch trampolines (bcond→jal→jal),
  loop-invariant constants rematerialized per-iteration (LICM gap),
  3–4 uncoalesced phi copies per trip. Same three levers the x64
  cross already climbed to gcc parity; the slow32 backend
  (hir_regalloc.h/hir_codegen.h) never got the June work.
- New divergence: stage08 cc emits `extern` globals as .bss
  DEFINITIONS (stdio.h → stdin/stdout/stderr defined per TU);
  stage08's linker merges common-style, the host linker correctly
  refuses. Compiler-side fix owed.

**Rulings (user, 2026-08-24):**
- **Struct-by-value: adopt clang's convention.** Every existing
  .s32o speaks it; stage08 conforms.
- **HW FP is in scope, not deferred.** stage08 cc must emit the
  real SLOW-32 FP instructions (FP on GPRs) and keep the
  transcendentals as intrinsic calls that slow32-dbt intercepts and
  hot-swaps to host intrinsics — the platform FP architecture, not
  the current `__fp64_*` soft-float.
- **Naming: say "stage08 cc".** "s12cc" is a fossil from a dead
  stage-12 staging breakdown; the file name stays, the prose stops.

Campaign order: codegen first (port the x64 cross's coalescing,
LICM-of-constants, branch layout into the slow32 backend; scoreboard
is inst/iter — ≤35 makes apps viable, 28.5 is parity), extern-def
fix folded in, then apps smallest-teeth-first, DOOM last, HW FP
when the first double-using app or the ABI gate forces the issue.

**Second strike (445afa1c): 34.4 → 30.1 inst/iter** — branch
trampolines replaced by direct branch shapes + a forwarding map,
and big loop constants promoted out of remat. Two latent traps
surfaced (both caught by the fixed-point gate, neither by the
55-test suite; both localized by per-function binary search over a
hybrid of good/bad gen1 assembly): (a) SLOW-32 bcond reaches only
±4096 bytes and the trampolines were accidental branch islands —
redirects are now distance-gated (hcg_bnear), and stage08's s32-as
was found not to range-check branches at all (owed); (b) the
call-crossing classifier used the kind-based remat predicate, so
promoted constants were colored caller-saved and died at the first
call. Remaining distance to LLVM's 28.5 ≈ loop rotation
(bottom-test) + LFTR.

**First strike landed same day (5f7bb023): 63.1 → 34.4 inst/iter**
(DBT cycles 561M → 188M; LLVM 154M — from 3.6× behind to 1.22×).
The copy plague was three stacked latent bugs — backprop liveness
over-extension (which had silently kept IRC coalescing from EVER
firing in this backend), missing coalesced-color propagation at
writeback, and per-param entry moves that clobber under register
permutation (fixed with a cycle-safe hoisted entry sequence; found
by strcmp comparing a string with itself). Gates: 56/56 incl.
fixed-point, LLVM interop PASS, checksum exact. Still open, named:
branch trampolines (bcond→jal→jal), loop-invariant constant remat
(lui+addi per iteration) — worth ~4 inst/iter on bench_arith,
i.e., most of the distance to LLVM parity.

## What not to do

- No JIT, no runtime codegen, no W^X exceptions. Ruled, permanent.
- No ISA changes in service of any leg.
- No dbt5/stage-5 speculation without a headroom-demonstrating
  workload (standing rule, reaffirmed).
- No parallel improvement of the LLVM backend — it is frozen except
  for bugs the differential harness finds. Improving both compilers
  at once destroys the oracle.
- No leg starts without its number: leg 1 starts with a profile,
  leg 2 has its baseline, leg 3's gates are enumerated above.

## Order of battle

Leg 1's profile first — it is a day, it informs everything, and the
daily driver feels it immediately. Then leg 2, measured against the
improved floor. Leg 3 advances app-by-app in the background on its
own gates, starting whenever a sitting wants it.
