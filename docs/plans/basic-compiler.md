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
- **Loading (2026-11-01):** a compiled program parses and lowers its
  source at start, so loading is the interpreter's: a third of a short
  game's run.  The lexer scans in place and hashes keywords; labels,
  builtins and procedures are found by hash (labels were quadratic);
  a statement is 216 bytes, not 1,184.  The 97 games, in
  instructions: interpreted 964M -> 530M, compiled 506M -> 298M
  (loading 96M of it).
- **FOR, RETURN, rounding (2026-11-01):** FOR is native (bounds, the
  counter stored as for_set stores it, the frame through
  `act_for_enter`, shared with the executor), so is a plain RETURN
  (`act_return`); a float becomes an integer through `val_rint`, not
  libm's rint (a quarter of compiled Mastermind); the C is built at
  -O2.  A compiled assignment to a CONST was stored (the cache it
  trusted is the reads'), and an interpreter NEXT stepped a CONST:
  both fixed, `tests/constset`.  The 97 games: interpreted 530M,
  compiled 236M (Mastermind 165M / 34M).
- **String functions (2026-11-02):** LEN, ASC, CHR$, LEFT$, RIGHT$ and
  MID$ are called through typed entries (`sb_mid`, ...; builtin.c's own
  functions call them too) with the arguments converted in C
  (`val_int_of`); one-character and empty strings are shared
  (`string_char`, `string_empty`), so `MID$(A$, I, 1)` allocates
  nothing; a string comparison is `strcmp` with a same-pointer
  shortcut; a string store moves its reference; `val_copy` and
  `VAL_CLEAR`'s string case are inline.  The 97 games: interpreted
  523M, compiled 224M (Mastermind 161M / 20M).  Loading, again: the lexer's character classes inline (each was a
  libc call), a line number's value without strtod, a label's name
  without snprintf: 96M -> 82M over the 97 games (the token streams of
  1,930 programs byte-identical before and after); interpreted 509M,
  compiled 210M.
- **DEF FN (2026-11-02):** a call of a numeric DEF FN defined once is
  inlined in native expressions: each argument stored into its
  parameter (a C local, converted as an assignment converts), the body
  evaluated with the parameters in scope over the unit's variables, the
  value converted as `val_coerce` converts it; the guard asks
  `eval_def_fn_defined` (cached until `array_generation` moves), so a
  call before the DEF is the interpreter's error.  The fuzzer defines
  four (shadowing globals, nested, one reading a string).  Salvo 7.9M ->
  4.6M; the 97 games compiled 204M.
- **RND (2026-11-02):** RND(n) is native, through `sb_rnd_of` (the
  generator's one entry, fn_rnd's too); 66 of the 97 games use it.  The
  97 games compiled 196M.
- **PRINT's TAB and SPC (2026-11-02):** a PRINT item TAB(n) or SPC(n)
  is native, `print_tab_spc` writing the spaces without a string (the
  column arithmetic `sb_tab_spaces`/`sb_spc_spaces`, fn_tab's and
  fn_spc's too); a PRINT of literals alone is native, so its strings are
  made once.  Checkers 11.5M -> 8.1M; the 97 games compiled 186M.
- **Output (2026-11-02):** the screen is written a run of printing
  characters at a time (`screen_puts`, wrapping where `screen_putc`
  would), and the runtime's stdio takes a line-buffered stream's
  newline-free writes on its short path: output had been ~100
  instructions a character.  The 97 games: interpreted 479M, compiled
  155M (3.09x).
- **Numbers printed (2026-11-02):** an integer, or a float that is a
  whole number of at most its precision's digits (7, 16), is written
  digit by digit, not through snprintf; print_value adds the sign's
  place without snprintf either.  Other floats keep `%.*e` for their
  rounding.  7.5M values formatted identically before and after (host
  differential; infinities, which every caller range-checks first,
  aside).  The 97 games: interpreted 470M, compiled 146M (3.22x).
- **Chunks, stage08 (2026-11-02):** a unit's ops are chunk functions of
  ~32KB of C sharing a frame (locals #defined over it), jumps out of a
  chunk through the unit's dispatch.  stage08 cc builds all 97 games
  (it compiled 31 before 24d11fa4/f5855539 and this), each printing what
  its clang build prints; the clang builds cost ~3% (150.7M against
  146M; 64KB chunks are 148.8M, 128KB exceed stage08's instruction
  table on two games).  The way to slow32:basic on the xbase pattern.
- **Open:** native procedures with arrays, strings, PRINT; the loop
  bodies' own constraints (GOTO in a procedure keeps it on the env path);
  loading (a third of a short game: the source is parsed at start).

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

## A compiler that decides up front (2026-11-03)

s32-sbc compiles everything the interpreter runs, by carrying the
interpreter's model into the compiled program: named environments, a
run-time FOR stack, values that know their type, native code behind
guards.  That bought 3.22x on the games and gets less with every step.
The next compiler is meant to decide a program's shape before it runs,
the way QuickBASIC's did, and to go to HIR directly -- a third user of
stage08's backend, beside fortran/ and COBOL's islands -- with the
string and formatting runtime from the C we have, so the output is the
interpreter's to the byte.

**Rulings (user, 2026-11-03).**  The interpreter and that compiler need
not run the same programs: the interpreter stays friendly to the
dialects that do not structure or type their code.  The weak form holds:
the interpreter runs everything the compiler takes.  s32-sbc stays, for
now; when the new compiler wins, the two are reconciled (a switch,
auto-detection, a build target).  QuickBASIC is the rule of thumb: line
numbers are labels (QB takes them, and ERL), not required, not refused.

**The checker** (`sbasic/src/strict.c`; `sbasic -check prog.bas`, and
`sbasic -strict prog.bas` runs a program only if it passes) is the
compilable dialect's definition, shared so that auto-detection later is
one call.  Its findings, which a compiler refuses:

- `next-without-for`, `for-without-next`: a NEXT with no FOR open before
  it in the text, a FOR that no NEXT closes;
- `label-twice`, `label-undefined`;
- `dim-twice` (an ERASE allows the next), `dim-after-use`;
- `fn-twice`, `fn-before-def`.

Everything else it counts as what a compiler can do from the text but
pays for: GOSUB (a return stack), ON ERROR, RESUME, RESUME NEXT and ERL
(resume points, the last numbered line), CLEAR, jumps into and out of
a FOR, a NEXT closing a FOR from a block inside it (`IF ... THEN PRINT
X$;: NEXT Z`, the "loop until" idiom), a NEXT skipping an inner FOR.

**What the survey said** (`sbasic/tests/strict-survey.py`, 399 programs:
the 97 games, NBS, fast/, sbasic/tests): 381 clean, 95%.  NBS's
findings are its exception tests -- the programs written to be refused
(P016's undefined label, P188's duplicate, P084's second DIM, P160's
second DEF).  What is left in real programs is one shape: two FORs of
one variable sharing a NEXT -- checkers.bas's `IF ... THEN FOR A=...`
in two branches over one `NEXT A`, awari.bas's second `FOR I` reaching
the first's `NEXT I` by GOTO -- and one DEF FN after its use
(bullfght.bas).

**Ruled (user, 2026-11-03): the compilable dialect is what QuickBASIC
loads** -- so every finding can be checked against QBasic 1.1 (and
QB64-PE, which agreed on every shape tried), and it is what people will
expect.  One deliberate difference: a DEF FN used before its definition,
or in its own, is taken -- the compiler defines them all first ("QB was
just being lazy").  QB's FOR/NEXT rule turned out stricter than the
first checker: a NEXT closes the innermost FOR open in its own block,
and nothing else -- `IF ... THEN PRINT X$;: NEXT Z` and a NEXT skipping
an inner FOR are "NEXT without FOR" in both; jumps into and out of a
loop are allowed.  So the compiler keeps a FOR's limit and step in
slots of that FOR's own, and every NEXT is a direct jump.

QuickBASIC is a full static compiler at load time -- it types every
expression, counts every call's arguments, folds constants -- so "what
QB loads" asks a good deal.  The checker's findings now:
`next-without-for`, `for-without-next`, `label-twice`,
`label-undefined`, `dim-twice`, `dim-after-use`, `dims-mismatch`
(subscripts other than the DIM's or the first use's), `dim-bound` (a
constant bound below OPTION BASE), `option-base` (twice, or after an
array), `fn-twice`, `fn-undefined`, `fn-args`, `fn-in-block` (a DEF FN
inside a block or a FOR), `builtin-args` (a builtin's argument count:
`SIN(1,1)`, `INT()`, a bare `TAN`; the counts are the ones each builtin
checks when it runs, now a column of builtin.c's table), `sub-undefined`
and `sub-args` (QB's "Subprogram not defined", which is what
sinewave.bas's `40 REMARKABLE PROGRAM BY DAVID AHL` is: GW reads a REM
there, QB a call).

`sbasic/tests/strict-qb.py DIR...` holds the checker to QBasic.  It asks
QB whether a program loads by running it with SYSTEM put first: a
program that loads ends at once, one that does not shows its load-time
error -- no keys (which pressed OK on that dialog in an earlier try), no
telling a message at load from the same one at run time.  Over
tests/strict/ (the shapes, one each), the 97 games, NBS and fast/: 349
agree, 4 are the hoisted DEF FNs, and the checker refuses nothing QB
loads.  QB refuses 14 the checker takes, three kinds left for the
compiler's front end, which must do them anyway: types (a string where a
number goes, 10 programs), a literal out of range (`3E-99999`, NBS
P034), and syntax -- empty parentheses (`RND()`, `FNP()`) and two
statements with no colon between them (P194).

## s32-bc (sbc/bc/, 2026-11-03)

The compiler that decides up front, begun.  `sbc/compile-bc.sh
prog.bas [prog.s32x]`; `sbc/build-bc.sh` builds `sbc/s32-bc` (host) and
`sbc/out/bc_rt.s32o`.

- **Front end:** sbasic's loader, lexer, parser and checker, linked on
  the host (the builtins' names and argument counts are one list now,
  `sbasic/src/builtins.def`, read by builtin.c for its functions and by
  builtin_names.c for the checker, so the host build needs none of the
  interpreter).  A program is compiled only if the checker passes it.
- **Back end:** a copy of stage08's (`sbc/bc/hir/`, at 4dc9b60a), as
  fortran/ and cobol/ keep theirs; `sbc/bc/bc_contract.h` is
  fortran's contract.
- **Values:** every operator, conversion and PRINT item goes through
  the interpreter's own code (`bc_rt.c`: eval_binary, eval_compare,
  eval_store, print_value), a value crossing as a double and a run-time
  type -- the type is a run-time one because some results' types are
  their values' (5.5 \ 2 is an INTEGER, 100000.5 \ 2 a LONG).  So the
  compiled program computes and prints what the interpreter does by
  construction; native code for the common cases comes after, held to
  this.  Variables, labels and control flow are native: a variable's
  slot (a double's two words) made and zeroed in the entry block; a FOR's
  limit and step in slots of its own, its NEXT a direct jump.
- **Milestone 0:** numeric scalars, assignment, PRINT (numbers, string
  literals, `;` `,`), IF, GOTO, labels and line numbers, FOR/NEXT,
  WHILE/WEND, DO/LOOP, END.  Anything else is "not yet: ...".
- **Gate:** `sbc/tests/run-oracle.py --bc` -- every program of the
  corpus through s32-bc and the interpreter, the same keys, the same
  bytes; refusals tallied by reason.  At milestone 0: 63 agree, 20
  refused where the interpreter refuses too, none differ; of the rest,
  what is "not yet" is the list of what to do next -- arrays and
  functions (99 programs), GOSUB/RETURN (49), strings (40), DATA/READ
  (35), INPUT (11).

**Milestone 1 (2026-11-03).**  GOSUB/RETURN (a return point's number on
a stack the depth of the interpreter's, every RETURN through one jump
table made last), builtin functions (builtin_call, numeric and string;
TAB and SPC as PRINT items), strings (a value crosses as three words --
a string's lo its sb_string_t, the interpreter's refcounted string --
and every value compiled code holds owns its reference), arrays
(eval_dim, eval_array_get, eval_array_store; static DIMs made before
the run as the scan makes them), INPUT, DATA, READ and RESTORE (the
interpreter's cores, now calls its own statements make: eval_input_line,
eval_read_value, eval_data_add), ON GOTO/GOSUB, DEF FN (inlined at each
call, parameters in slots of the call's own).  The interpreter's start
of a program is one call now, eval_reset, which the runtime makes too.
DEF FN is defined before the program runs in the interpreter as well
(its scan registers each), so it runs what the compiler takes.

`run-oracle.py --bc`: 327 agree, 20 refused as the interpreter refuses,
none differ; of the rest, about 40 are refused by the checker (QB would
not load them) and about 30 are "not yet": SUB/FUNCTION/CALL, SELECT
CASE, EXIT, ON ERROR/RESUME, DEFINT and its kind, SWAP, PRINT USING.

Speed, the 92 games both compilers take: interpreter 417M instructions,
s32-sbc 137M, s32-bc 223M -- every operator still a call into the
interpreter's code.  Native paths next: a static type for each
expression where the program fixes one, integer arithmetic with QB's
Overflow checks inline, doubles and singles through the hardware, each
held to the runtime's answer.
