# Growing s32-xbc into the SLOW-32 xBase compiler

Status 2026-10-30: the plan's steps are done but one (typed LOCALs,
waiting on a measurement), and the goal moved past it.  On 2026-10-29 the user set the bar at CA-Clipper 5.2 -- "a tool
someone who used Clipper would recognize" -- so the work since is
compliance, tracked in `xbc/docs/compat-survey.md` against the CA
guides with Harbour as the oracle.  This page keeps the plan as it was
written, with what became of each step; the survey is where the open
work is.

## Where it stands (2026-10-30)

- **s32-xbc** (3,200 lines of host C, plus `xbc/runtime/cliprt.c`)
  compiles a whole program -- the files it DOes, SET PROCEDURE, the
  preprocessor (all ten directives, #command's full grammar), code
  blocks, objects (Error, Get, TBrowse, TBColumn) -- to C, linked with
  the interpreter's runtime.  CA's own utilities run compiled: DBU, RL
  (reports and labels), PE (MEMOEDIT), with ERRORSYS and GETSYS.
- **The interpreter** (xbase/, 33,000 lines) is the oracle for the
  dBASE side and runs the same runtime; `report_clipper_layout()` gives
  compiled programs Clipper's REPORT FORM, LABEL FORM and READ.
- **Gates:** the xbase suite (110), the xbc oracle (54 agree;
  `xbc/tests/run-oracle.sh`, which now types keys for programs that read
  the keyboard), the compat probes against Harbour (68), CA's driver
  programs (13), and the stage08 build (`xbc/tests/run-stage08.sh`).
- **Use:** the user's ledger (majesty) runs its dBASE drivers compiled
  since 2026-10-30, 14.5 s to 3.6 s; `slow32:xbase` is published, the
  interpreter and `s32xbc` on base with stage08 cc and no LLVM.

## The shape it grows into

- **Front end: xbase's own parser.**  `xbase/src/ast.c` already parses
  expressions to an AST; s32-xbc links it on the host and parses every
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
gives the same output compiled as interpreted.  Steps 0-4 and 6 are
done (6: stage08 builds the whole of xBase since selfhost ISSUES-103,
held to clang by `xbc/tests/run-stage08.sh`); 5 is not started, and
waits for a measurement that asks for it.

0. **The oracle harness.**  Corpus: `xbc/tests`, the 36 `.PRG` files
   in `xbase/tests`, and the xbase test scripts that are plain command
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
- (Clipper 5's code blocks, objects and preprocessor were listed here
  "until a real program needs one"; the Clipper bar of 2026-10-29 made
  them in scope, and all three are done.)
- Changing the interpreter to suit the compiler: where they disagree,
  the interpreter's answer is the definition, unless it is a bug, which
  is then fixed in both.
