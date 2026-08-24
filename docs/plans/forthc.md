# forthc — the native Forth compiler

Leg 2 of the engine room (engine-room.md). Written 2026-08-23, the
night leg 1's profile proved the thesis this compiler rests on:
Forth's cost on this machine is NEXT dispatch, it is *serial*, and
no DBT-side cleverness removes it. Only deleting the dispatch does.

## Rulings already made (do not relitigate)

- **W^X is not negotiable.** No JIT, no runtime code generation,
  ever. forthc is AOT: `.fth` in, object code out, executed as an
  ordinary `.s32x`.
- **The DTC kernel stays forever.** It is the stage01 bootstrap seed
  and the REPL where words get invented. forthc is a new artifact,
  not a replacement.
- **The interactive compromise is the turnkey image** —
  SAVE-SYSTEM's descendant: compile the program, emit `.s32x`, run
  it. Development happens at the DTC `ok>`; shipping happens through
  forthc.
- **The scoreboard is ours.** Baseline (M5 Max, heavy bench,
  startup-corrected): DTC-under-dbt 260 ms; gforth-fast 66 ms.
  Win condition: **≥2× over DTC under the same DBT.** Stretch:
  gforth-fast parity. The honest metric is our own before/after —
  the gforth ratio is partly environmental.

## The design

### Hosted in Forth, on the kernel

forthc is a Forth program (`forthc.fth`) run on the DTC kernel, the
way `asm.fth` and `cc.fth` are. This is not sentiment; it is reuse:
stage01's object-emission machinery — the words that write `.s32o`
sections, symbols, and relocations, and `link.fth`'s `.s32x`
emission — already exists, in Forth, proven by the bootstrap purity
guard. forthc's back half is written; the project is the front half.

### Compilation model: subroutine-threaded, primitives inlined

- **Primitives inline.** DUP is one instruction pattern, not a call:
  the compiler carries a template per primitive (the same bodies the
  kernel's assembly implements, minus the `jal r0, next` tail).
- **Colon words are native subroutines.** A call is `jal r31, word`;
  `EXIT` is `jalr r0, r31, 0`. The return stack is the machine's
  return address plus an explicit r-stack for `>R`/`R@`/loop
  parameters, exactly as the kernel lays it out.
- **Register model matches the kernel**: DSP in r28, RSP in r27,
  same stack discipline — so a compiled program's memory picture is
  familiar, and a future mixed mode (compiled words calling DTC
  words) stays possible without being promised.
- **Structure words compile to real branches.** IF/ELSE/THEN,
  BEGIN/UNTIL/WHILE/REPEAT, DO/LOOP become `beq`/`bne`/`jal`
  targets, not calls. This is where the dispatch deletion actually
  happens: a DO/LOOP that cost two dispatches per turn costs one
  fused compare-branch.
- **v1 is closed-world.** One program, fully compiled, `MAIN` (or
  the last word) as entry. No dictionary at runtime, no
  interpretation, no DOES> in v1 (CREATE/ALLOT data space yes —
  it is just `.data`).

### Why the DBT will love the output

Leg 1 measured the DBT running straight-line compiled code at ~0.67
host cycles per guest instruction, and Forth-DTC at ~5× worse than
that ceiling because every few instructions end in an indirect jump.
forthc's output is ordinary call/branch code — the exact shape the
DBT chains, superblocks, and (now) select-fuses. The 2× win
condition is deliberately conservative; the instruction-count
reduction alone (no NEXT loads, no XT fetches, fused loop control)
should approach 3–4× on dispatch-bound code.

## Milestones — each one session-sized, each with a gate

- **M1 — hello.** forthc compiles literals, a dozen inlined
  primitives (stack ops, `+ - * @ ! C@ C!`, `EMIT`, `.`), and `:`
  definitions into a linkable object; `hello.fth` → `hello.s32x`
  prints on all engines. *Gate: byte-for-byte identical output from
  slow32 / slow32-fast / slow32-dbt.* Directory `forthc/` is earned
  here. **LANDED 2026-08-23, the same evening as the charter** —
  `forthc/forthc.fth` (~330 lines of Forth), gate green on all three
  engines on the first compiled program's first run. Emits `.s` text
  exactly as cc.fth does; lui/addi constant synthesis; the dot
  routine rides `debug`.
- **M2 — control flow and calls.** IF-family, BEGIN-family,
  DO/LOOP/+LOOP/I/J/LEAVE, colon calls, RECURSE, `>R R> R@`.
  `fib-only.fth` compiles. *Gate: fib(32) = 2178309 everywhere, and
  a first honest fib measurement vs DTC.*
  **LANDED 2026-08-23, same sitting as M1.** Structure words emit
  real labels and branches (text output makes forward references
  free); DO/LOOP inlines the kernel's exact boundary-cross test;
  comparisons are single SLT-family instructions (the kernel's 0/1
  flag convention, mirrored for M4). Every m2.fth case and fib(32)
  correct on the first compiled run, three engines byte-identical.
  First measurement (fib 32, medians of 5): compiled-under-dbt 34 ms
  compute vs DTC-under-dbt 121 ms — **3.6×, the M3 win condition
  already exceeded** — and within 1.2× of native gforth-fast (28 ms)
  with zero optimization: framed leaves, memory stack, no peephole.
  Guest instructions: 226M vs the DTC's 585M.
- **M3 — the bench.** CREATE/ALLOT, FILL, VARIABLE/CONSTANT,
  comparison + logic vocabulary — everything `bench.fth` needs.
  *Gate: the win condition. Heavy bench compiled vs DTC-under-dbt,
  medians of five, ≥2× or a written explanation.*
  **LANDED 2026-08-23, same sitting as M1 and M2.** Top-level
  CREATE/ALLOT/VARIABLE/CONSTANT/`,` ride forthc's own data stack
  (the metacompiler's oldest trick); data lands in a `.data` buffer
  appended in the postamble; typed word-table entries make a name
  mean call, address-push, or literal. bench.fth's kernels compile
  verbatim and print 317811/1899/1000000 on the first run, three
  engines identical. **The measurement (heavy bench, medians of 7,
  wall):** compiled-on-dbt 86 ms; DTC-on-dbt 271 ms; gforth 102 ms;
  gforth-fast 85 ms. Win condition met at **3.8×**; plain gforth
  beaten; **statistical dead heat with native gforth-fast — the
  stretch goal, unoptimized.** Counters: we retire 2.8× more host
  instructions (2.93G vs 1.05G) in 4.6% more cycles — IPC 9.1 vs
  3.4; the M5 absorbs the translation surplus and the race is
  critical paths, where compiled Forth is gforth's equal.
- **M4 — the differential.** Enough of the prelude to compile the
  kernel test suite's programs; run each `.fth` both ways (DTC vs
  compiled), outputs must match exactly. *Gate: the forth suite's
  programs, 26/26, identical output.* This is forthc's version of
  the reel: the DTC kernel becomes the oracle.
  **LANDED 2026-08-23, same sitting (26/26 amended to what closed
  worlds can honestly mean).** Additions: implicit MAIN (top-level
  statements compile into a synthesized entry, so unmodified kernel
  test files feed straight in, with a pending-number queue for
  compile-time values), `prelude-fc.fth` — the prelude re-hosted in
  compilable Forth, stealing the oracle's own colon definitions
  verbatim (division family, doubles, strings) — plus
  kernel-verbatim primitives (2!/2@/S>D/D+/D-/UM*/M*/2>R-family/
  DEPTH/PICK/EXECUTE/UNLOOP, UM/MOD as an emitted routine), VALUE/TO,
  DEFER/IS/'/[']/ACTION-OF (function pointers via `jalr`),
  2VARIABLE/2CONSTANT, CASE family, S"/."/TYPE, and pictured-lite
  (decimal `<# #S #>`, `D.`, `.R` family). *Result: 15 of 26 suite
  tests compile and match the DTC oracle byte-for-byte; 0
  divergences; the 11 skips are enumerated and principled* —
  interpreter-domain tests (runtime `:`, ticking kernel words,
  PARSE/WORD/PAD, wordlists, BASE/HEX, `[ ]`), the kernel's own
  error-message test, and the tube test (M5's linking work). The
  differential caught two real slips en route: a prelude ordering
  bug and SEARCH returning ANS -1 where the kernel returns 1 — the
  oracle works.
- **M5 — turnkey and the showpiece.** A driver script (`forthc
  prog.fth prog.s32x`), docs, and one demo with teeth: `ship.fth`
  compiled — the vec arcade at native speed, tube words inlined.
  **LANDED 2026-08-23 — five milestones, one charter, one day.**
  The new machinery is **hosted mode** (`compile.sh --hosted`):
  forthc emits `main` under crt0 + libc_mmio instead of its own
  `_start`, preserving the C callee-saved registers it repurposes —
  and because r27/r28 *are* callee-saved, the tube words are the
  kernel's own ten-line C-call wrappers as inline templates
  (TUBE-INIT/OPEN/CLOSE/PRESENT/INFO/STATUS/KEYS, MS via usleep).
  HEX/DECIMAL landed as dual-effect words (compile-time parse radix
  + runtime FBASE store, with `n BASE !` literal folding), which
  brought the differential to **18/26 matched, 0 diverged** — the
  8 remaining skips all interpreter-domain. `demo/ship.fth` is the
  compiled flyable ship; the gate (`tests/run-tube-frames.sh`) runs
  the same scene script — two headings, thrust, the DEFER-retargeted
  pinwheel — through both worlds: **4 frames, compiled == DTC,
  hash-identical, first try.** The arcade is native now.
- **Parked until pulled:** DOES>, mixed compiled/DTC execution,
  cross-word inlining, peephole on emitted SLOW-32, forthc
  compiling itself (delicious, not load-bearing).

## Where forthc does not go (ruled 2026-08-23, day of completion)

**Not into selfhost/stage01, ever.** The user's argument, endorsed
and sharpened: the stage01 tools are run-once batch work with no
runtime to win (the full leg rebuilds in under a second); using
compiled tools *adds* a forthc compile step or a cached artifact
with its own regeneration and sum-verification burden — net slower;
`asm.fth`/`cc.fth` are interpreter-hosted by design and outside the
closed world; and above all, the bootstrap's tiny trusted seed
(s32-emu + kernel.s32x + prelude.fth) is the purity argument itself
— inserting a compiler between seed and toolchain enlarges the
auditable base for zero benefit. The DTC kernel's slowness in
stage01 is a feature: it is the simplicity you can read. forthc is
a *product* of the bootstrap, never a component of it.

## What not to do

- No runtime codegen, no dictionary-in-the-image, no "small
  interpreter for the tricky words" — closed-world means closed.
- No new kernel primitives for forthc's benefit; the kernel is the
  seed and stays still.
- No optimizing the emitted code before M3's measurement exists.
  The idiom-contract lesson applies: measure, then shave.
- No relitigating W^X, ever.
