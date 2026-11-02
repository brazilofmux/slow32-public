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

## Where it stands (2026-11-01)

- **M0 and M1 done.**  The pipeline (a5094b24); expressions, assignment,
  PRINT, IF conditions, GOTO and GOSUB compiled on the interpreter's
  operations (83d15a36); variables through cached value pointers, with
  `env_generation` saying when they go stale (ba609dd4); SUB and FUNCTION
  bodies as C functions (38228235).
- **M2 under way.**  Numeric assignments, IF conditions, NEXT and array
  elements have native versions behind type guards (c10647ba, 624e186f,
  657a1ea2).  Instructions, interpreter against compiled: nested loops
  443M / 35M, the sieve 4.50M / 1.24M, fib(20) 71.6M / 71.5M.
- **Gates:** `sbc/tests/run-oracle.py` (393 agree, 25 refused by both, of
  418: sbasic's tests and the NBS, games and feature collections), and
  `sbc/tests/fuzz.py` (random typed arithmetic, loops and arrays over
  edge values; 2000 of 2000; mutations of the native code caught).
- **Calls (2026-11-01):** a call of one of the program's FUNCTIONs or
  SUBs takes its arguments from the C -- natives too, in a native
  expression, with the guard's pointers checked again after the call --
  and goes to `eval_call_proc`; a call's scope and entries come from a
  pool, its setup is done once per procedure, and ending a call's scope
  no longer invalidates the caller's caches.  fib(20): 100M (the
  interpreter, before) -> 69M interpreted, 55M compiled (with the
  by-reference parameters).
- **Native procedures (2026-11-01):** a FUNCTION or SUB whose body is
  all native also has a C version, variables in C, parameters as
  `value_t *` (by reference the caller's variable, by value a converted
  temporary), FOR/NEXT paired in the text; the env version stays for
  everything else.  fib(20): 2.27M instructions compiled, 69.3M
  interpreted.
- **Builtins and strings (2026-11-01):** the numeric builtins are C in
  native expressions; string variables, elements and literals, `+`, the
  comparisons and the string functions are native in the main program
  and the env procedures (the interpreter's `val_add`, `val_compare`,
  `builtin_call`, no env lookups).  The 97 games: 964M instructions
  interpreted, 506M compiled (530M before strings); Mastermind 364M /
  68M (92M before).
- **Open:** native procedures with arrays, strings, PRINT; the loop
  bodies' own constraints (GOTO in a procedure keeps it on the env path).
  PRINT of values; `FOR` itself (only `NEXT` is native).

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
