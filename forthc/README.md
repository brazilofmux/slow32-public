# forthc — the native Forth compiler

Charter and milestones: `docs/plans/forthc.md`. Status: **M3 landed.**

forthc is a Forth program (`forthc.fth`) that runs on the DTC kernel
the way `asm.fth` and `cc.fth` do, reads a closed-world Forth source
file, and emits SLOW-32 assembly text: subroutine-threaded colon
words, inlined primitives, its own `_start`, stacks, and dot routine.
The output links standalone — no kernel, no libc, no dispatch loop.

```bash
bash compile.sh prog.fth prog.s32x     # .fth -> .s -> .s32o -> .s32x
bash tests/run-tests.sh                # the gate: 3 engines, identical bytes
```

Vocabulary through M2: `: ; MAIN`, decimal literals, comments,
`DUP DROP SWAP OVER + - * AND OR XOR 1+ 1- @ ! C@ C! EMIT CR .`,
comparisons `= <> < > <= >= U< 0= 0< 0<>` (kernel 0/1 flags),
`IF ELSE THEN`, `BEGIN AGAIN UNTIL WHILE REPEAT`,
`DO ?DO LOOP +LOOP I J LEAVE`, `>R R> R@`, `EXIT`, `RECURSE`.

M3 adds top-level `CREATE ALLOT VARIABLE CONSTANT ,` (compile-time
values ride forthc's own stack) and `FILL`; bench.fth compiles
verbatim. The measurement (heavy bench, medians of 7): compiled 86 ms
vs DTC 271 ms (3.8×), gforth 102 ms (beaten), gforth-fast 85 ms —
**dead heat with native gforth, unoptimized**. M4 next: the
differential — the kernel suite compiled vs DTC, outputs identical.
