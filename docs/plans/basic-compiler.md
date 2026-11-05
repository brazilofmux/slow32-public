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

## The two compilers, reconciled (2026-11-04)

The ruling of 2026-11-03 kept s32-sbc "for now" and said the two would
be reconciled when the new compiler won -- a switch, auto-detection,
or a build target.  It has won where it applies: on the 92 games both
take, s32-bc runs 84.5M instructions to s32-sbc's 181M (the
interpreter's 469M), the faster on 89 of them, and it compiles 96 of
the 103 QB-corpus programs QBasic loads.  It does not apply
everywhere, and was never meant to: it takes what QBasic loads, and
the interpreter -- with s32-sbc -- stays friendly to the dialects that
do not structure or type their code.  Of the 97 games the interpreter
runs (GW-BASIC programs, line-numbered), s32-bc takes 92 as they are;
the 5 it refuses QBasic refuses too -- a FOR and its NEXT in different
blocks (animal, awari, checkers), a DEF FN inside a FOR (salvo), a word
after a line number that QBasic reads as a SUB's call (sinewave).

So they are two compilers for two languages, named for them:

- **s32-bc, the QuickBASIC compiler** -- the compilable dialect, what
  QBasic 1.1 loads (the checker its definition, held to QBasic by
  strict-qb.py), compiled up front through HIR;
- **s32-sbc, the SLOW BASIC compiler** -- everything the interpreter
  runs, compiled to C with the interpreter's executor beside it.

The reconciliation is auto-detection: `sbc/compile.sh prog.bas` tries
s32-bc and, when it refuses, compiles with s32-sbc and says why
(`--qb` and `--any` choose one).  sbc/README.md is their common
description.  And slow32:basic ships both (2026-11-04): `s32bc`,
`s32sbc`, and `s32basic` (compile.sh) -- s32-bc and its bc_rt.s32o
built in the image's toolchain stage, both compilers linking
libsbasic.s32a; s32-bc writes assembly, so its programs need no C
compiler in the image at all.

Those numbers are after a repair: the text-screen model (the SCREEN()
work, 2026-11-03) copied 2 x 23 x 256 bytes on every scroll and cleared
512 on each, and took every program from about 68M instructions to 371M
(s32-bc; the interpreter 755M, s32-sbc 467M), with all the gates green,
because all of them compare output.  A scroll now moves row numbers and a
cleared row is a length of 0.  `compare-speed.py --check` holds the
totals to speed-baseline.txt, within 5%.

**s32-bc on its known types (2026-11-04).**  Kernels in sbc/tests/kernels
(one concern each, a checksum printed) say what the games, mostly output,
cannot; `compare-speed.py sbc/tests/kernels`.  Strings go to typed entries
with a literal made once and a variable borrowed when nothing after it can
run the program's code.  A loop whose body can do nothing to an array
(region_for: only listed statements, no label, none of the program's code)
holds a settled array's data and bounds in values made before it: a mirror.
Settled: never ERASEd or REDIMmed, and made before the run or by a top-level
DIM with no jump before it.  NEXT drops its overflow checks when the counter
is INTEGER and the step too (the 32-bit one), and when only NEXT moves it and
limit + step is known in range (both).  None of this in a program that traps
errors, where any op can be entered again.

The same mirrors hold in WHILE and DO loops.  A main-program array's cell is
good across calls: array_dim_generation moves only when an array is made,
erased or re-dimensioned, where array_generation moves with every scope (what
a procedure's own names mean changes with it).  PRINT of a known string or
integer goes straight to the screen (bcrt_print_str, bcrt_print_int,
scr_put_run), the interpreter's too.

The fuzzer met s32-bc that day (`tests/fuzz.py --bc`, programs QBasic would
load, half with no handler; `--loops`, loops above all, with a refusal tally
so "all agree" cannot be vacuous, and mutation-tested: a mirror's bounds check
removed shows in 39 of 200, the counter rule in 3 of 600).  It found, QBasic
deciding each: RETURN without GOSUB is trapped (the interpreter stopped); a
subscript is made an integer as it is evaluated (s32-bc did them all first);
a value is held to its type's range only when stored, an expression carries
infinity on; passing a variable by reference to another type is a load error
(param-mismatch); `name (x)` with no CALL passes by value.

A NEXT whose FOR did not run (a jump into the body, a start that overflowed,
resumed past) has the limit 0 and the step 1 if none is written, else 0, a
literal too.

Known and left, each pathological: nested on one counter, with the inner FOR
failed, the interpreter's NEXT finds the outer loop's frame by name.  QBasic's `7 ^ (32767 + g#)` raises Overflow
and takes the THEN too, and a store after an overflowing ^ leaves 1.#INF in the
variable; inf - inf stores a NaN without error.

A value is held only to the range of what it is stored in: past a SINGLE's and
into a DOUBLE it goes whole (`x# = a! / b!` is 1.3D+43, QBasic), and only past
a DOUBLE's is it an Overflow.  (943b9bed had this backwards for s32-bc,
Overflow at a SINGLE's limit; `tests/native/past-single`.)

ERASE and CLEAR leave what QBasic leaves (`tests/native/erased`).  A static
array -- a main-program DIM of numbers and CONSTs, or an implicit one -- stays
dimensioned and is zeroed, records and STRING * n elements as DIM made them.
A dynamic one -- REDIM, a DIM of anything else, any array of a procedure, its
STATICs too -- is gone: a use (subscripts evaluated first) or UBOUND is
Subscript out of range until DIM or REDIM makes it again.  A DIM of other
bounds after an ERASE of a static array is GW-BASIC's, and QB's load error;
the interpreter still makes it anew.  `'$DYNAMIC` is not read yet: QBasic makes
`DIM m(3)` under it dynamic, here it stays static (item 4 of the road ahead).

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
interpreter's code.

### Native paths (2026-11-03)

A value whose type the program fixes has it statically, and lives
natively: an INTEGER or LONG its int, a SINGLE or DOUBLE its double
(a SINGLE unrounded between operators, as the interpreter keeps one),
a string its sb_string_t; the three words a runtime function takes are
made only when one does.  + - * of integers are inline with value.c's
Overflow (an INTEGER's sum checked to 16 bits, a LONG's by the sign
rule, a LONG product by mulh); a float among them, and /, are the
hardware's doubles (stage08's backend makes __fp64_* single
instructions), / checked for zero; comparisons, AND/OR/XOR/IMP/EQV/NOT,
negation, \ and MOD by a constant, and conditions, the same.  A store
does store_into's work: INTEGER range, a SINGLE rounded after the
FLT_MAX check, a DOUBLE's infinity an Overflow.  FOR keeps its limit
and step in their own types, in words zeroed at the start (a NEXT whose
FOR never ran sees QBasic's limit 0 and the step as written when it is
a literal: NBS P055), and NEXT steps the variable as for_next does.
Arrays stay the interpreter's (LBOUND, ERASE, REDIM see them), but an
element is reached through a cell per name in the runtime -- data,
base, dimensions -- good while array_generation is the cell's (as
s32-sbc caches them), with the bounds checks inline.  What is not
fixed still goes through the runtime (builtins' results, POW, strings'
operators), and its type is set where value.c's rules make it one.

sbc/tests/native holds the edges, each run-time error a program of its
own that must end in it.  `run-oracle.py --bc`: 355 agree, 20 refused
alike, none differ.  The 92 games: s32-bc 113.7M instructions, against
s32-sbc's 137.3M and the interpreter's 417M (mastermind, near half of
it, went from 79M to 48M when its arrays stopped being name lookups).

Then (74dab7db, 7f0a53de, ad0444ee): a float made an integer inline
(truncate, convert back, compare -- exact on every engine though
fcvt.w.d out of range is Unspecified -- else bcrt_d2i's range and
val_rint), which a SINGLE subscript takes on every access; builtins'
results typed where builtin.c fixes them (with builtin_call's rule: a
double result is a SINGLE unless a DOUBLE went in), INT, FIX, ABS,
SGN, CINT, CLNG, CSNG, CDBL and RND computed in place; RANDOMIZE
(eval_randomize, shared).  The 92 games: 66.6M -- half of s32-sbc's
137.3M, a sixth of the interpreter's 417M.  What is left in the
profile is the compiled code itself (variables of a large main live in
the frame, the backend's own choice; the double helpers' register
shuffles), string comparisons and builtins, PRINT.

### The constructs (2026-11-03)

DEFINT and its kind (26c97e8a): compiled where each DEFtype is in no
block and comes before every name of its letters -- there the
interpreter's types (made when a variable is first made) and
QuickBASIC's (by the text) agree.  Three interpreter fixes on the way,
each against QBasic: DIM after DEFINT made a SINGLE array; DEFSTR S:
READ S was a Syntax error; an FN was typed by its F.  SELECT CASE, EXIT
FOR/DO/WHILE and SWAP (15e6441d).  SUB, FUNCTION and CALL (e383baee):
each procedure a function of its own, every parameter a pointer (the
caller's variable, a copied element, or a temporary), SHARED and STATIC
globals; and from QBasic, a FUNCTION of no parameters named alone is
called, and two -check findings (proc-twice, call-function).
`run-oracle.py --bc`: 388 agree, 20 refused alike, none differ.

Then (user, 2026-11-03: "QuickBASIC-style per-procedure arrays, then ON
ERROR/RESUME"): a procedure's arrays are its own unless SHARED A(), DIM
SHARED or STATIC (d7e2bce9: array.c scopes, an activation's arrays
erased as it returns; -check array-undefined, QB's "Array not defined";
s32-bc 3051012c, with DIM SHARED and scalar DIM AS); ON ERROR GOTO,
RESUME, ERROR n, ERR, ERL in s32-bc (fa4c1880: the statements a
function of their own, re-entered at the handler after a longjmp; ops
numbered as lower.c's so RESUME and RESUME NEXT land where the
interpreter's do).  `run-oracle.py --bc`: 399 agree, 20 refused alike,
none differ.

Open:
- A DIM with constant bounds makes a static array in QuickBASIC, which
  REDIM refuses; the interpreter resizes it.
- QuickBASIC refuses at load what it can type statically (Type
  mismatch, Parameter type mismatch); -check does not yet (strict-qb's
  DIFF-QB, 43 over the corpus).
- Not yet: GOSUB inside a procedure, ON ERROR and RESUME inside one,
  a whole array as an argument, a record as a whole (assigned, passed).

Then (2026-11-03, "Let's continue"): CONST (2a39b385/c31245a3), the
console statements and PRINT USING as a state machine both drive
(2347da3a/42782dd0), files (f60ad3a8/27d226c0), graphics
(76477a94/d728f5d6), LPRINT, directories, POKE (4e0f1818/950c9822),
TYPE records (ee0c2b26/a513017c) -- each statement's action a function
of the interpreter's that compiled code calls too.  QBasic showed, and
the interpreter now follows: CONST lists, a module CONST in every SUB,
EOF true when the next read finds nothing, DIM AS a declaration (not
reset in a loop), DIM SHARED records.  -check gained const-assign,
const-value, not-qb (TRACE, SPLIT, OPTION EXPLICIT), dim-after-use and
dim-twice for DIM AS.  `run-oracle.py --bc`: 435 agree, 20 refused
alike, none differ.

Interpreter deviations from QuickBASIC seen and left: PRINT USING's
signs and fields; a name with a dot (TWO.PI#) does not lex; REDIM
resizes a static array.

### The QB corpus (2026-11-03)

`sbc/tests/qb-corpus.py [-l]` runs s32-bc over QB64-PE's QBasic test
cases (~/refs/basic/qb64pe/tests/qbasic_testcases, not in the tree),
the 100 of 143 that QBasic 1.1 loads, and counts what stops each: the
parser, the checker, or a "not yet" (-l: the line).  Worked down from
59 compiling: DIM a(lo TO hi) (67626669/70362fd7), the QB forms of
LOCATE, WIDTH, INPUT # and LINE INPUT (c62464ec/d191b573), then arrays
of records and STRING * n (1007f26b/64321ea4/49151c3f) -- 61 compile.
QBasic showed, on the way: a record assigned is copied (the
interpreter had shared it); a fixed-length string starts as n CHR$(0)s
and every store cuts or pads it, a SUB's by-reference one included
when the call returns; A(i) = e evaluates e before the subscripts.
s32-bc's per-function table of failure blocks held four kinds of error
unchecked: vektor.bas overran it (now 32, checked).  The 92 games:
67.8M instructions (from 66-67.5M: e first costs a little).

Then graphics GET and PUT (35d4d75a/d8a3cfcf): QuickBASIC's array
layout per mode, checked in QBasic's SCREEN 7 and 1 (its DOS emulator
has no VGA; 13 and 12 are held to the layout as documented, and
compiled to interpreted); HEX$ of a negative INTEGER its 16 bits on the
way.  68 compile.  Seen and left: A! under DEFINT is SINGLE in QBasic
(done since: DEFtype written into names); SCREEN 0 after SCREEN 7 keeps
40 columns there.

Then the small ones (3b4a1bee/5e9a060a): the loader's 2048-line limit
(it went on with what it had: "DO without LOOP" 4,000 lines on), a
label only at a line's start, FUNCTION f% named f, GET #/PUT # and
MID$ = of elements and records, PMAP, VARSEG; BSAVE, BLOAD and CALL
ABSOLUTE known (QB's "Advanced feature unavailable" here; PEEK and POKE
then reached a flat 64K -- the machine's addresses came later, below);
the checker's array parameters, ERASEd arrays,
and RESTORE in a SUB.  72 compile, no checker refusals left.

Then VIEW and WINDOW (with PMAP, POINT both ways, CLS n), checked in
SCREEN 7: 77 compile.  QBasic's CIRCLE keeps the mode's aspect (a
radius of 20 is about 17 pixels up and down in SCREEN 7); ours is round
in pixels -- the CIRCLE item below is every program's, not only arcs.
Then CIRCLE fitted to QBasic's pixels (arcs, aspect; 79 compile).  Its
LINE breaks ties otherwise than ours, in both directions -- a single
Bresenham offset does not fit the four lines measured; open.

Then the parser's forms (677e1ae8: names with a dot, RUN, SUB ...
STATIC, STATIC x AS type, one-line ELSEIF, label 1.1, SCREEN , , p, p)
and GOSUB in procedures (s32-bc): 84 compile.  Then whole arrays as
arguments (87 compile), DEFtype as the text's: the parser writes each
name's suffix, the run-time DEFtype gone (89); RUN in s32-bc (91); DRAW (92); the text screen -- SCREEN(), VIEW PRINT,
WIDTH's rows -- (93).  Left: ON ERROR in a SUB (s32-bc), the four
programs of the machine's memory, and two QB64 programs QBasic loads
only because its _ continues a line.

The machine's memory, reviewed (2026-11-04): what is DOS and x86 in
QuickBASIC splits in two.  Machine code -- CALL ABSOLUTE, CALL
INTERRUPT, code BLOADed or POKEd and run -- is not done and will not
be: SLOW-32 is W^X, and running it would mean another processor's
interpreter inside; it stays "Advanced feature unavailable".  The
PC's well-known places a program reads and writes as data are done
(sbasic/src/machine.c, shared by both compilers through the
interpreter's objects):

- PEEK/POKE at DEF SEG * 16 + offset, wrapped at 1MB; an offset past
  -32768..65535 is Overflow.  The first 640K keeps what is POKEd
  (made at the first POKE); A000 is the mode-13 frame (POINT and PSET
  see it); B800 the text screen's cells, character then attribute
  (SCREEN() sees them); 0:400 the BIOS data area -- shift flags kept,
  the video mode, columns and rows, the tick count; the rest reads 0.
- OUT/INP/WAIT: the VGA DAC (3C7 read index, 3C8 write index, 3C9 the
  components, six bits each, three to a colour -- PALETTE's table
  changes with it), 3DA's retrace bit by the clock at 70 a second, so
  WAIT &H3DA, 8 waits a frame; any other port reads 255 and takes
  nothing, and WAIT there looks once rather than hang.  QBasic's
  ranges, probed: a port -65536..65535, a value or mask an INTEGER
  (its low byte used), past them Overflow.

sbasic/tests/pcmem.bas is held to QBasic byte for byte (text mode);
so is sbc/tests/native/vgaports.bas (SCREEN 13, the DAC, A000), since
the DOS emulator's VGA (2026-11-04), which also showed the BIOS's 40
columns in modes 1, 7 and 13.  Not done, and
not asked: arrays at addresses (VARPTR/VARSEG into a model of DGROUP,
BSAVE/BLOAD of an array), CP437 output, DOS paths.

The text screen on a VGA (2026-11-04, the emulator's VGA): 43 and 50
rows, and text in the graphics modes, each probed and pinned in
sbasic/tests/textrows.bas and screenmodes.bas (both byte for byte with
QBasic).  WIDTH's rows are 25, 43 or 50, and 30 or 60 in SCREEN 11 and
12; anything else is Illegal function call; WIDTH , rows is a form.  A
line filled on the last row, below the viewport, leaves the cursor at
the viewport's bottom and scrolls nothing.  LOCATE past the columns is
refused.  Every SCREEN clears the text and homes the cursor; a mode's
text is 40 columns in 1, 7 and 13 and 80 in the rest, 30 rows in 11 and
12; SCREEN 0 after a graphics mode takes its columns, 25 rows; WIDTH 40
in SCREEN 11 or 12 is refused.  SCREEN 11 is there (12's size, 2's
one-bit image layout).

Then WIDTH in the graphics modes, and the colors (widthmodes.bas, byte
for byte with QBasic): WIDTH 80 makes SCREEN 1 into 2, 7 into 8, and 13
into text (SCREEN 0, 80 columns); WIDTH 40 makes 2 into 1, 8 or 9 into
7; a change clears, the mode's own width changes nothing.  Each mode
has its colors -- 4 in SCREEN 1, 2 in 2 and 11 -- and a color drawn is
its low byte, the mode's last if past them (SCREEN 1's 4 to 255 all 3;
256 is 0); POINT in text mode is Illegal function call.  PALETTE takes
QBasic's values at last: an attribute the mode has; at 200 lines (1, 2,
7, 8) one of the 16 colors, in 9 (and text) any of the 64 EGA colors,
at VGA (11, 12, 13) blue * 65536 + green * 256 + red, six bits each;
-1 leaves it.  graphics.bas had PALETTE 4, &HFF8000 in SCREEN 1, which
QBasic refuses twice over, and colors past 3 that it draws as 3.

Then errors in procedures (cr.bas: ON ERROR in a SUB; 94 compile).
QBasic's handler is the main program's code, but an error in a SUB or
FUNCTION runs it with the procedure still there, and RESUME and RESUME
NEXT go back into the procedure; RESUME label leaves it.  The
interpreter had RESUME land after the CALL.  Now act_transfer runs the
handler (the main program's activation, from its label) where the
error is; RESUME label unwinds with ERR_RESUME_LABEL.  s32-bc: in a
program that traps, each procedure is a wrapper (bcp_NAME, the ABI's)
and a body, bcb_NAME(area, at), whose variables are words of an area
the wrapper keeps in its frame -- area[0] the op it is at -- run by
bcrt_proc_run, which setjmps in C; RESUME longjmps there and the body
is entered again at the op, as bcm_body is.  (Not a setjmp in compiled
code: HIR keeps frame slots in registers.)  s32-sbc makes no native
procedures in such a program.  On the way, each checked in QBasic
(procerr.bas, errstate.bas, errgoto0.bas, native/trapprocs.bas):
COMMON SHARED shares (it was ignored: four corpus programs lost every
SUB's changes); ERR is 0 after any RESUME, ERL stays; ON ERROR GOTO
another label in the handler keeps it handling, GOTO 0 there stops the
program with the error; out of stack in a procedure is not trapped; an
untrapped ERROR n prints its error's message (it printed "ON ERROR
GOTO"), "Unprintable error" for a number with none.

Then COMMON SHARED's arrays (commonarr.bas): a(), b() AS INTEGER, a
record's, STRING * n's -- each the main program's in every procedure
once its DIM (after) makes it, through REDIM in a SUB and ERASE.  The
parser makes them a SHARED list at the module's level (QBasic has no
other way to write one), which the interpreter's scan, s32-bc's
dimshared_array and the checker take as DIM SHARED's; a COMMON's AS
type is its REDIM's without AS, and a DIM must say AS again ("AS clause
required"; another type "Duplicate definition" -- as-required and
common-type, the checker's).  On the way, CHR$(0) as QBasic has it:
PRINT showed nothing from it on (C strings); on the screen it is a
blank (SCREEN() reads 32), to a file the byte, and INPUT # and LINE
INPUT # pass it over.  And nbs.load_probe passes TYPE blocks too
before its SYSTEM, which had made every COMMON after a TYPE a refusal.

Then the last of the order QBasic evaluates in (concatorder.bas): in
s$ + e, a variable-length string variable s$ is read as the two are
joined, after e -- so s$ = s$ + f$, f$ a FUNCTION that changes s$,
appends to s$ as f$ leaves it, and s$ + f$ + ":" too; s$ + ":" + f$
does not (s$ + ":" is made first), nor an array's element, a
fixed-length string, or a number.  All three take e first there
(reading s$ does nothing else): the interpreter's EXPR_BINARY,
s32-sbc's and s32-bc's lazy_concat.

The load-probe fix (above) found three corpus programs QBasic loads
that had been counted out.  For them: records as parameters (x AS a
TYPE, a() AS a TYPE -- the interpreter had refused both, and s32-bc
copied a record element out and back where the callee changes it in
place); arrays of four to eight dimensions in s32-bc (bcrt_dim_n,
bcrt_elem_n, bcrt_elem_back_n: the subscripts in a block); the
checker's DIM twice only where QBasic refuses it (the main program,
constant bounds); and LBOUND/UBOUND with no DEFtype suffix
(dims4.bas, recparams.bas).

Where it stands (2026-11-04): 96 of the 103 corpus programs QBasic
loads compile.  The seven left are five that use the machine itself
(CALL ABSOLUTE, and BLOAD/BSAVE of code or of an array's memory --
not to be done, W^X) and two that only load because QB64's _
continues a line.  Everything QBasic was asked along the way is
pinned in a test that prints what QBasic prints.

Then QBasic's load-time checks (2026-11-04), the last of what the
checker took that QBasic does not: strict-qb now agrees on every
program (398, the 4 hoisted DEF FNs apart; it had been 14 short).
QBasic types the whole program as it loads it, so the checker does
too (strict.c's ty_check): each expression a number, a string, or not
told (a record, another dialect's builtin), and a definite mismatch
is type-mismatch -- an operator's operands, a comparison's, an
assignment's two sides, an array's subscripts and element, a
condition (IF, WHILE, DO), FOR's counter and bounds, ON's index, PRINT
USING's format, LOCATE's and COLOR's operands, SWAP's two, a builtin's
arguments (a table of QBasic's, each its result and its arguments'
types; LEN of a number not a variable is variable-required), a SUB's
or FUNCTION's or DEF FN's arguments against their parameters, a DEF
FN's value against its name.  Names are typed by their suffix, else
by a declaration in their scope -- the procedure's own, or the main
program's where the procedure sees it (DIM SHARED, COMMON SHARED,
CONST, its own SHARED) -- else as SINGLEs.  Also: empty parentheses
on a builtin, a DEF FN or a FUNCTION (RND(), FNP(), f(): "Expected:
expression"), a statement right after another with no colon between
(end-of-statement; IF c GOTO n is one statement, and DATA's rest is
its own), and a number whose first significant digit is below
10^-32751 (3E-99999; 12E-32752 loads) or an n& past a LONG, refused as
the parser refuses other literals out of range.  The tests/strict
pairs pin each.  s32-bc now refuses what QBasic refuses at load: the
native err-* tests (run-time type errors) are QBasic load errors, and
onerror.bas raises its Type mismatch with ERROR 13 now.

Older list, by count: SCREEN
with arguments left out, names with a dot
(SUB summer.day, GOSUB LOAD.SPRITES), SUB names the interpreter has as
commands (BYE, SAVE, NEW, RUN), SUB ... STATIC, STATIC x AS type, a
whole array as an argument (the parser now notes array parameters),
GOSUB in a SUB, a DEFtype after a use, ELSEIF on a line
of its own after a single-line THEN, SCREEN() as a function.  Also
from QBasic: a FUNCTION f% may be named f, inside and out (the
interpreter makes f a variable).

