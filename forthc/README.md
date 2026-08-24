# forthc — the native Forth compiler

Charter and milestones: `docs/plans/forthc.md`. Status: **M2 landed.**

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

First measurement (fib 32, dbt, medians of 5): compiled 34 ms vs
DTC 121 ms — 3.6×; within 1.2× of native gforth-fast, unoptimized.
M3 next: CREATE/ALLOT/VARIABLE/CONSTANT and the full bench.
