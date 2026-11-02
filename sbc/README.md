# s32-sbc — the SLOW BASIC compiler

A `.bas` becomes a `.s32x`: SLOW BASIC ([sbasic/LANGUAGE.md](../sbasic/LANGUAGE.md))
compiled for SLOW-32.  The plan: [docs/plans/basic-compiler.md](../docs/plans/basic-compiler.md).

s32-sbc loads, parses and lowers the program with the interpreter's own
front end (`sbasic/src`: `program.c`, the parser, `lower.c`) and writes
the flat op list as C -- each op a label, a jump a `goto` -- linked with
the interpreter's runtime, whose executor (`act_step`, `act_transfer`)
runs what the C does not do itself.  The program's source is embedded;
at run time it is lowered again by the same code, and the C checks the
ops are the ones it was written from.

```bash
./compile-bas.sh prog.bas [prog.s32x]     # after sbasic/build.sh
../tools/emulator/slow32-fast -q prog.s32x
python3 tests/run-oracle.py -j 8          # the gate
```

`tests/run-oracle.py` is the gate: every program of the corpus run by
the interpreter and compiled must print the same bytes, with the same
keys on stdin.  The corpus is sbasic's tests and the NBS, games and
feature collections in `~/refs/basic/nbs-ecma55-test` (outside the
tree).  2026-11-01, milestone 1 (expressions, assignments, PRINT, IF
conditions, GOTO and GOSUB compiled; the rest the executor's): 391
agree, 25 refused by both, of 416; nested loops 495M -> 419M
instructions against the interpreter.
