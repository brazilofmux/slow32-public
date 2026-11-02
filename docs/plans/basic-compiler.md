# s32-sbc, the SLOW BASIC compiler

Started 2026-11-01, after the dialect was written down
([sbasic/LANGUAGE.md](../../sbasic/LANGUAGE.md): QBasic 1.1 the
authority).  The model is s32-xbc ([xbase-compiler.md](xbase-compiler.md)):
a host program that runs the interpreter's own front end at compile time
and writes C, linked with the interpreter's runtime, the interpreter
the oracle.

## Shape

- **Front end: the interpreter's.**  s32-sbc links sbasic's lexer,
  parser, AST and lowering (`src/lower.c`) on the host.  A program is
  loaded by the same code (`src/program.c`: lines, `$INCLUDE`) and
  lowered to the same flat op list the interpreter runs.
- **The op list becomes C.**  Every op is a label; `OP_JUMP` is a
  `goto`, a condition an `if`, and a transfer whose target is known only
  when it runs (`RETURN`, `RESUME`, `ON ... GOTO`, `NEXT`) a `switch`
  over the op indices.  Line-numbered BASIC's jumps anywhere are what C's
  `goto` does.
- **The runtime: the interpreter's.**  The generated program links
  sbasic's objects minus `main.c`.  Its source text is embedded; at
  start it is parsed and lowered again, by the same code, so the
  runtime holds the same ops the C was written from (checked).  What is
  not compiled is run by the interpreter's own executor on those ops.
- **One executor for both.**  `run_code`'s work per op and its handling
  of transfers and errors are functions the generated code calls
  (`act_*` in eval.c), so the compiled program cannot drift from the
  interpreter in what it does not compile itself.

## Milestones

- **M0 -- the pipeline.**  The main unit's ops as labelled C; each
  statement still run by the executor.  Gate: every program prints what
  the interpreter prints (`sbc/tests/run-oracle.sh`), over the unit
  tests, NBS, the games and `fast/`.
- **M1 -- expressions and the common statements in C.**  Assignment,
  PRINT, IF conditions, FOR limits: C calls on the interpreter's value
  operations, as s32-xbc does.  SUB and FUNCTION as C functions.
- **M2 -- types.**  BASIC's types are static (suffix, DEF*type*, DIM
  AS), so an INTEGER can be a C `short`, a SINGLE a C `float` computed
  in double: native code where the types allow, the runtime elsewhere.

## Gates

The interpreter is the oracle for the compiled program, and QBasic 1.1
the oracle for both (`sbasic/tests/nbs.py`).
