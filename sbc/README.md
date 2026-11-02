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
agree, 25 refused by both, of 416.  Variables are reached through
cached pointers to their values (good while `env_generation` holds);
SUB and FUNCTION bodies are C functions too.

Milestone 2 (started): a numeric assignment or IF condition has a native
version -- C `long long` and `double`, the interpreter's rules for each
type written out -- run when its guard holds (every variable it names
cached and of the type its name predicts), its generic version
otherwise.  NEXT steps its own loop natively when that loop is on top
and its variable cached; elements of the arrays the program DIMs are
read and stored natively too (pointers cached while `array_generation`
holds); calls of the program's FUNCTIONs and SUBs take their arguments
from the C (native ones too) and go straight to the call
(`eval_call_proc`).  Nested loops: 36M instructions against the
interpreter's 443M; the sieve 1.26M against 4.50M; fib(20) 50.4M
against 61.3M.

```bash
python3 tests/fuzz.py -n 1000      # random typed arithmetic, compiled vs interpreted
```

`tests/fuzz.py` generates programs of random arithmetic over edge values
in every numeric type, conditions, FOR loops (edge bounds and steps,
nested, EXIT FOR), arrays (every type, one and two dimensions, edge
and fractional subscripts, ERASE and REDIM) and calls (typed FUNCTIONs,
one recursive, a SUB with SHARED), errors trapped and printed, and requires the compiled
program to print what the interpreter prints (2000 of 2000 on
2026-11-01; moving the INTEGER overflow bound by one in the native code
is caught, and so is a bounds check one off -- in half the programs --
and it found the native comparison of a NaN).
