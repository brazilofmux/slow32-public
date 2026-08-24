# forthc — the native Forth compiler

Charter and milestones: `docs/plans/forthc.md`. Status: **M5 landed — the charter is complete.**

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

The measurement (heavy bench, medians of 7): compiled 86 ms vs DTC
271 ms (3.8×), gforth 102 ms (beaten), gforth-fast 85 ms — **dead
heat with native gforth, unoptimized**.

M4/M5: implicit MAIN compiles unmodified kernel test files;
`prelude-fc.fth` re-hosts the prelude in compilable Forth;
VALUE/DEFER/tick, doubles, strings, CASE, S"/.", pictured with a
runtime BASE. `tests/run-differential.sh`: **18/26 kernel-suite
tests compile and match the DTC oracle byte-for-byte, 0
divergences** (8 skips, all interpreter-domain). `--hosted` mode
links under crt0 + libc_mmio with the tube words as the kernel's own
C-call wrappers; `demo/ship.fth` is the compiled flyable ship, and
`tests/run-tube-frames.sh` proves it: the same scene script through
DTC and compiled worlds produces **hash-identical tube frames**.
