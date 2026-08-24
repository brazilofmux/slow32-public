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
  here.
- **M2 — control flow and calls.** IF-family, BEGIN-family,
  DO/LOOP/+LOOP/I/J/LEAVE, colon calls, RECURSE, `>R R> R@`.
  `fib-only.fth` compiles. *Gate: fib(32) = 2178309 everywhere, and
  a first honest fib measurement vs DTC.*
- **M3 — the bench.** CREATE/ALLOT, FILL, VARIABLE/CONSTANT,
  comparison + logic vocabulary — everything `bench.fth` needs.
  *Gate: the win condition. Heavy bench compiled vs DTC-under-dbt,
  medians of five, ≥2× or a written explanation.*
- **M4 — the differential.** Enough of the prelude to compile the
  kernel test suite's programs; run each `.fth` both ways (DTC vs
  compiled), outputs must match exactly. *Gate: the forth suite's
  programs, 26/26, identical output.* This is forthc's version of
  the reel: the DTC kernel becomes the oracle.
- **M5 — turnkey and the showpiece.** A driver script (`forthc
  prog.fth prog.s32x`), docs, and one demo with teeth: `ship.fth`
  compiled — the vec arcade at native speed, tube words inlined.
- **Parked until pulled:** DOES>, mixed compiled/DTC execution,
  cross-word inlining, peephole on emitted SLOW-32, forthc
  compiling itself (delicious, not load-bearing).

## What not to do

- No runtime codegen, no dictionary-in-the-image, no "small
  interpreter for the tricky words" — closed-world means closed.
- No new kernel primitives for forthc's benefit; the kernel is the
  seed and stays still.
- No optimizing the emitted code before M3's measurement exists.
  The idiom-contract lesson applies: measure, then shave.
- No relitigating W^X, ever.
