# sbc — the BASIC compilers

A `.bas` becomes a `.s32x`.  There are two compilers, for two
languages, and one interpreter both are held to (`sbasic`; its
language, [sbasic/LANGUAGE.md](../sbasic/LANGUAGE.md)).  The plan and its
history: [docs/plans/basic-compiler.md](../docs/plans/basic-compiler.md).

| | **s32-bc** — the QuickBASIC compiler | **s32-sbc** — the SLOW BASIC compiler |
|---|---|---|
| takes | what QBasic 1.1 loads: the checker (`sbasic -check`, `sbasic/src/strict.c`) passes it -- typed, every FOR its NEXT, every label there | anything the interpreter runs: GW-BASIC's line numbers and habits, ECMA-55, the interpreter's own statements |
| how | decides the program's shape before it runs and writes native code through HIR (stage08's back end, `bc/hir/`); values the run time types go through the interpreter's own code (`bc/bc_rt.c`) | writes the interpreter's lowered ops as C, native behind guards, the interpreter's executor running the rest; C by clang or stage08 |
| speed (the 92 games both take, instructions; `tests/compare-speed.py`) | 72.9M | 181M (the interpreter: 469M) |
| sources | `bc/bc.c`, `bc/bc_rt.c`, `build-bc.sh`, `compile-bc.sh` | `s32-sbc.c`, `runtime/`, `build.sh`, `compile-bas.sh`, `build-lib.sh` |
| shipped | `slow32:basic` (`s32bc`; assembly straight out, no C compiler) | `slow32:basic` (`s32sbc`, stage08 cc on the emulator, no LLVM) |
| gate (both also `tests/compare-speed.py --check`) | `tests/run-oracle.py --bc`, `tests/fuzz.py --bc [--loops]`, `tests/qb-corpus.py` (QBasic's programs), `sbasic/tests/strict-qb.py` (the checker against QBasic) | `tests/run-oracle.py`, `tests/run-stage08.sh`, `tests/fuzz.py` |

Both print what the interpreter prints, byte for byte: that is each
one's gate.  The interpreter runs every program either compiler takes.

In the slow32:basic image the same three are `s32basic`, `s32bc` and
`s32sbc` (each `prog.bas [out.s32x]`).

```bash
./compile.sh prog.bas [prog.s32x]        # s32-bc if it takes the program, else s32-sbc (it says which, and why)
./compile.sh --qb prog.bas               # s32-bc only: a refusal is an error
./compile.sh --any prog.bas              # s32-sbc only
```

`compile.sh` is the reconciliation the plan ruled for when s32-bc had
won (2026-11-03: "a switch, auto-detection, a build target"):
auto-detection by trying s32-bc, whose refusal -- a checker finding,
QBasic's own reason, or a "not yet" -- names the line.  As of
2026-11-04 s32-bc compiles 96 of the 103 QB-corpus programs QBasic
loads; the seven left use the machine itself (CALL ABSOLUTE, BLOAD,
BSAVE) or are QB64's.

## s32-bc — the QuickBASIC compiler

`compile-bc.sh prog.bas [prog.s32x]`; `build-bc.sh` builds `s32-bc` (a
host program: sbasic's loader, lexer, parser and checker, and the HIR
back end) and `out/bc_rt.s32o`.  It compiles a program only if the
checker passes it -- the compilable dialect is what QuickBASIC loads,
with one difference on purpose (a DEF FN may be used before its
definition) -- and says "not yet: ..." for what it does not do.  The
run time is the interpreter's: every operator, conversion, PRINT item,
file, graphics and error goes through the interpreter's own code, so
the program computes and prints what the interpreter does by
construction; native code covers the common cases, held to it.
Variables, labels and control flow are native.  In a program that
traps errors the main program's statements are a function entered
again at an op (bcm_body), and so is each procedure's body (bcb_NAME,
its variables in an area its wrapper keeps), so RESUME returns into a
SUB as QuickBASIC's does.  The design and its steps:
[docs/plans/basic-compiler.md](../docs/plans/basic-compiler.md) ("s32-bc").

## s32-sbc — the SLOW BASIC compiler

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
otherwise.  FOR evaluates its bounds and enters its loop natively, and
NEXT steps its own loop natively when that loop is on top and its
variable cached; a plain RETURN returns natively; elements of the arrays the program DIMs are
read and stored natively too (pointers cached while `array_generation`
holds); calls of the program's FUNCTIONs and SUBs take their arguments
from the C (native ones too) and go straight to the call
(`eval_call_proc`).  Nested loops: 36M instructions against the
interpreter's 443M; the sieve 1.26M against 4.50M; fib(20) 2.27M
against 69.3M.  Arguments are passed as QBasic passes them: a variable by
reference (`eval_arg_ref`, its type checked as it is reached), anything
else by value; a call with an array element among its arguments is the
interpreter's.  A FUNCTION or SUB whose body is all native (numeric
assignments, conditions, FOR/NEXT, EXIT, calls; no DEF* in the program,
no GOTO, GOSUB, SHARED, STATIC, arrays, strings or I/O) also gets a C
version with its variables in C and no env (NF<n>), which a call site
runs when it may (no TRACE, none of its variables named as a builtin).
A call of a numeric DEF FN is inlined (its parameters C locals, its
body native).  Numeric builtins (ABS, INT, SQR, SIN, CINT, ...) are C in native code;
strings are native too: a string variable or element is read as a copy,
literals are made once, `+` is the interpreter's `val_add`, a
comparison is `strcmp` (one string equal to itself without it), a store
moves its reference; LEN, ASC, CHR$, LEFT$, RIGHT$ and MID$ go to their
typed entries in builtin.c (`sb_mid`, ...), the other string functions
straight to `builtin_call`.  Strings of one character and the empty
string are shared, not allocated (`string_char`, interpreter too).  The 97 games:
964M instructions interpreted, 506M compiled; Mastermind 364M against
68M.

A unit's ops are written as chunk functions of about 32KB of C each
(S32SBC_CHUNK sets the size; 0 is one function), sharing a frame of the
unit's locals: a jump within a chunk is a goto, one out of it returns to
the unit's dispatch.  One function of a whole program's ops was more than
stage08 cc can compile (it keeps a table per HIR instruction), and the
chunks are what lets the self-hosted kit build these programs: all 97
games build with stage08 and print what the clang builds print.

```bash
python3 tests/fuzz.py -n 1000      # random typed arithmetic, compiled vs interpreted
S32SBC_CHUNK=1 python3 tests/fuzz.py -n 300   # every op its own chunk: every jump crosses one
```

`tests/fuzz.py` generates programs of random arithmetic over edge values
in every numeric type, conditions, FOR loops (edge bounds and steps,
nested, EXIT FOR), arrays (every type, one and two dimensions, edge
and fractional subscripts, ERASE and REDIM) and calls (typed FUNCTIONs,
one recursive, by-reference parameters, a SUB with SHARED), strings
(concatenation, the string functions and comparisons over variables,
elements and literals), errors trapped and printed, and requires the compiled
program to print what the interpreter prints (2000 of 2000 on
2026-11-01; moving the INTEGER overflow bound by one in the native code
is caught, and so is a bounds check one off -- in half the programs --
and it found the native comparison of a NaN; concatenating in the
wrong order is caught in 140 of 200).
