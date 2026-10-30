# Growing s32-clip into the SLOW-32 xBase compiler

Status 2026-10-29: steps 0-2 done -- the oracle harness
(`clip/tests/run-oracle.sh`: 45 of 45 compiling programs agree), the
rest of the control flow, and compiled expressions (7.6x the
interpreter's speed under slow32-dbt on `clip/tests/bench1.prg` scaled
up).  Step 4 under way: program files reached by DO compile in, and
SELECT, SKIP, GO, APPEND BLANK, SEEK and REPLACE are direct calls; a real
report suite runs 2.9x-3.6x faster per phase than interpreted, same
output.  Variable lookup became a hash index in the interpreter itself
(both gain).  The plan as written follows.

dbase/ is a well-developed dBASE III Plus interpreter (23K lines, 109
tests, Clipper and FoxPro additions, the last dBASE III Plus stubs
closed 2026-10-29), and it only interprets.  In the period, compiling
dBase programs was what Clipper was for.  `clip/` (s32-clip) is the
first slice of that: a `.prg` becomes a `.s32x`.  This plan grows it
into the compiler, one piece at a time, with the interpreter as the
oracle throughout.

COBOL stays the main tool for the user's own work; dBase is what you
use when you have no COBOL.  So the bar is "compiles real dBase
programs and runs them faster, with the same answers", not "complete
Clipper 5".

## Where it stands

s32-clip (951 lines, host C) reads a `.prg` line by line and writes C,
which clang compiles for SLOW-32 and links with the dbase sources minus
the REPL (`clip/runtime/cliprt.c` is the glue):

- Compiled: assignment, `STORE`, `?`/`??`, `IF/ELSEIF`, `DO WHILE`,
  `DO CASE`, `EXIT`/`LOOP`, `FUNCTION`/`PROCEDURE` with
  `PARAMETERS`/`PRIVATE`/`LOCAL`/`RETURN`, `SET PROCEDURE TO` inlined.
- Not compiled: every expression is still a string handed to the
  evaluator at run time (`clip_let(name, "expr")`, `clip_truth("expr")`),
  and every other command rides `clip_cmd()`, the interpreter's own
  command dispatch, re-lexed each time it runs.
- Compile errors: `FOR`, `SCAN`, `TEXT`, `BEGIN SEQUENCE`, `DO ... WITH`.

The real-program check so far: eleven of twelve of the user's report
programs give byte-identical output compiled and interpreted (the
twelfth hits an interpreter IF-stack limit, not a compiler fault).

## The shape it grows into

- **Front end: dbase's own parser.**  `dbase/src/ast.c` already parses
  expressions to an AST; s32-clip links it on the host and parses every
  expression at compile time.  One grammar for both, so the two cannot
  drift.
- **Values: the interpreter's `value_t`.**  Character, numeric, date,
  logical and memo, dynamically typed, as in dBase.  Operators become
  calls into the runtime's value operations, the same code the
  interpreter runs, which is what makes the interpreter a strict oracle.
- **Variables.**  `LOCAL` (and `STATIC`, when it comes) become C
  variables of `value_t`.  `PRIVATE`/`PUBLIC` stay in the memvar store:
  their dynamic scoping is semantics, not overhead.  An undeclared name
  is a field of the current work area or a memvar, decided at run time
  as dBase does; `alias->field` and Clipper's `FIELD`/`MEMVAR`
  declarations let the compiler decide early.
- **Macros stay in the runtime.**  `&name` and expression strings built
  at run time need an evaluator; it ships in every image, as it did in
  Clipper.  A program without macros never calls it.
- **Commands.**  The frequent ones (`USE`, `REPLACE`, `SKIP`, `GO`,
  `SEEK`, `APPEND BLANK`, `SELECT`) get direct runtime calls with
  arguments compiled once; the rest keep `clip_cmd()` until a program
  needs them faster.
- **Back end: C, through clang now and stage08 cc too.**  The
  infrastructure brought to bear is the two C compilers (clang's
  optimizer; stage08 cc as the self-hosted path, and a second compiler
  to differ against), not a third copy of a backend.  Emitting HIR
  directly, the way s32-cobc does, stays open if profiling ever shows C
  in the way; nothing here forecloses it.

## Steps

Each step ends with the oracle green: every program in the corpus
gives the same output compiled as interpreted.

0. **The oracle harness.**  Corpus: `clip/tests`, the 36 `.PRG` files
   in `dbase/tests`, and the dbase test scripts that are plain command
   sequences (most are; `CREATE`'s field prompts are the exception).
   Each is run by the interpreter (`DO name`) and compiled, outputs
   compared; the user's own programs join as a local-only leg, never
   copied into the tree.  The count of programs that compile is the
   progress measure.
1. **The rest of the control flow.**  `FOR/NEXT`, `SCAN/ENDSCAN`,
   `TEXT/ENDTEXT`, `BEGIN SEQUENCE/BREAK/RECOVER`, and `DO ... WITH`
   with dBase's by-reference passing of variables.
2. **Expressions compiled.**  Parse with ast.c at compile time; emit
   calls on `value_t`; fold constants.  Names resolve at run time
   except `LOCAL`s.  Macros fall back to the evaluator.
3. **Variables.**  `LOCAL` and `STATIC` as C variables;
   `FIELD`/`MEMVAR` declarations; `alias->field` with the field index
   looked up once.
4. **Commands lowered,** by frequency in the corpus.
5. **Types where they are known.**  A `LOCAL` that only ever holds a
   number becomes a double, and so on; measure first.
6. **stage08 cc as the second back end.**  Build the corpus with both
   compilers and require the same output (the way SQLite and DOOM are
   held), which also tests stage08 on code it has never seen.

## Measures

- Programs compiled out of the corpus, and the interpreter/compiled
  agreement over all of them (the gate).
- Speed against the interpreter on the user's report programs (the
  interpreter's own history: 46.5 s to 12.7 s in September 2026).

## Not in scope

- `.NDX` compatibility with real dBASE: our NDX2 is fine, and reindexing
  is the interchange, as it was in the period (ruled 2026-10-29).
- Clipper 5's code blocks, objects and preprocessor, until a real
  program needs one.
- Changing the interpreter to suit the compiler: where they disagree,
  the interpreter's answer is the definition, unless it is a bug, which
  is then fixed in both.
