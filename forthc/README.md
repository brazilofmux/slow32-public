# forthc — the native Forth compiler

Charter and milestones: `docs/plans/forthc.md`. Status: **M1 landed.**

forthc is a Forth program (`forthc.fth`) that runs on the DTC kernel
the way `asm.fth` and `cc.fth` do, reads a closed-world Forth source
file, and emits SLOW-32 assembly text: subroutine-threaded colon
words, inlined primitives, its own `_start`, stacks, and dot routine.
The output links standalone — no kernel, no libc, no dispatch loop.

```bash
bash compile.sh prog.fth prog.s32x     # .fth -> .s -> .s32o -> .s32x
bash tests/run-tests.sh                # the gate: 3 engines, identical bytes
```

M1 vocabulary: `: ;` definitions and calls, decimal literals (full
32-bit range), `DUP DROP SWAP OVER + - * @ ! C@ C! EMIT CR .` and
`\`/`( )` comments. `MAIN` is the entry point. Control flow is M2;
the bench and the ≥2×-over-DTC win condition are M3.
