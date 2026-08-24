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
2. **The tube demos and the reel.** Frame hashes are
   compiler-independent truths — a stage08-built reel must produce
   the same 14 golden hashes. First cross-compiler conformance test
   that costs nothing to run.
3. **The ABI divergences stop being footnotes.** Double-argument
   passing and struct-by-value are "known-divergent, not gated" in
   the interop proof. Taking over means picking conventions and
   closing them.
4. **DOOM.** Built by stage08, `-timedemo demo3` runs to completion —
   and because the game logic is fixed-point-deterministic, the 2173
   frame hashes should match the clang build's goldens exactly. Same
   answer from a different compiler: the final boss, again.

LLVM is not deleted at the end of this. It becomes the reference —
the differential oracle stage08 is measured against, the way gforth
oracles stage01. "Taking over" means default, not exile.

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
