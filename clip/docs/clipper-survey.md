# clip against CA-Clipper 5.2: the first survey

Status: survey, 2026-10-29.  The goal (user, 2026-10-29): a tool someone
who used Clipper would recognize -- Clipper 5.2 behaviour, not COBOL
speed.  Summer '87 programs are the subset they are.

## Method

- **Authority:** the CA-Clipper 5.2 guides (Computer Associates, 1993),
  held locally in `~/refs/clipper` and never copied into this tree; this
  page names their entries and paraphrases, nothing more.
- **Oracle:** Harbour, an independent open-source reimplementation that
  aims at Clipper 5.2/5.3 compatibility (`Dockerfile.harbour-oracle`,
  pinned).  It answers the small questions by running them; the guides
  win where it departs from Clipper.
- **Probes:** `tests/clipper/probes/<area>-<slug>.prg`, original programs
  of one feature each; `tests/clipper/run-probes.sh` compiles each with
  s32-clip and runs it beside Harbour.  Verdicts PASS, DIFFER, NOCOMPILE,
  ORACLE (Harbour refuses it: the probe is wrong).
- The interpreter (dbase/) stays the oracle for what dBASE III and
  Clipper share (`tests/run-oracle.sh`).  Where they differ, a behaviour
  point is recorded below and decided once: the dBASE dialect stays the
  default, Clipper's behind a switch, unless the two agree that the
  interpreter is simply wrong.

## The inventory (the language guide's menus)

| Section | Entries | Present in clip/dbase by name |
|---|---|---|
| Functions | 220 | all 219 names by 2026-10-29 (the batch after step 9; the approximations are listed below) |
| Commands | 110 | nearly all by first word; the database and program clauses probed 2026-10-29 (below) |
| Statements | 22 | the dBASE ones (DO, IF, FOR, PRIVATE, PUBLIC, PARAMETERS, RETURN, BEGIN SEQUENCE, LOCAL in part) |
| Operators | 28 | the dBASE ones |
| Directives (preprocessor) | 10 | all ten in s32-clip (step 7), #command's full grammar; the interpreter has none |
| Classes (Error, Get, TBrowse, TBColumn) | 4 | all four (steps 8, 9); the `:` send operator |
| Get System | 10 | the Get class, GETNEW, READMODAL, READVAR, UPDATED, READEXIT / READINSERT / READKILL, GETACTIVE over dbase's @ SAY/GET/READ (step 9) |
| Drivers (RDD) | 31 | DBF/NDX2 only, by design (NDX ruling 2026-10-29) |

The missing functions fall in clusters: arrays (AADD, ASIZE, ATAIL,
AFILL, ACLONE, AEVAL, ARRAY), code blocks (EVAL, AEVAL, DBEVAL,
FIELDBLOCK, MEMVARBLOCK, ERRORBLOCK), the DB*() functional forms of the
database commands, introspection (PROCNAME, PROCLINE, PCOUNT, VALTYPE),
screen and keyboard (SAVESCREEN, RESTSCREEN, SETCOLOR, SETPOS, DISPBOX,
NEXTKEY, SETKEY), memo text (MEMOLINE, MLCOUNT, MEMOREAD, MEMOWRIT),
binary conversion (BIN2I, I2BIN, ...), and output (QOUT, QQOUT, DEVOUT,
OUTSTD).

## The first probes (29), 2026-10-29: 1 PASS

Everything else differs, and for a few causes, not 28:

| Cause | Probes |
|---|---|
| Operators the parser does not have: `:=`, `!=`, `#` as not-equal in all places, `+=` and the other compound assignments, `++`/`--`, `@` by reference | op-inline-assign, op-not-equal-forms, op-compound-assign, op-increment, op-byref, stmt-for-step (`FOR i := ...`) |
| `{ }`: literal arrays and code blocks | op-array-literal, op-codeblock, fn-aeval, fn-array-ops |
| A function call as a statement (`Say("x")`), formal parameters in the header with PCOUNT() | stmt-procedure-return, stmt-function-params, op-byref |
| LOCAL / STATIC / PUBLIC / PRIVATE with initializers; STATIC at all | stmt-local-init, stmt-static, stmt-public-private |
| Functions missing (IF, VALTYPE, PROCNAME, QOUT, EVAL, ERRORBLOCK ...) | fn-iif-if, fn-valtype, fn-procname, fn-qout, fn-errorblock |
| The preprocessor | dir-define, dir-ifdef, dir-command |
| BREAK with a value, RECOVER USING | stmt-sequence-break-value |
| MEMVAR / FIELD declarations, M-> | stmt-memvar-field-decl, op-alias-assign |
| Numeric display (below) | most probes that print a number |

## Behaviour points

- **BP-Q1, `?`'s newline.**  Clipper writes the newline before the values
  (`?` is QOUT: new line, then the list); the dBASE dialect here writes it
  after.  The probe harness moves Harbour's to the end so the probes
  compare content.  **Decided 2026-10-29: newline first everywhere**
  (user), the interpreter and clip alike -- dBASE III did the same.  ?
  is QOUT(): its arguments are evaluated first, then the new line, then
  the output (so `? PCOL()` sees the column the last output left); a
  failing argument prints nothing.  Console messages (text that ends a
  line, through dbase's printf wrapper) start a line of their own when ?
  left one open, and so does the dot prompt.
- **BP-N1, numbers carry a width and decimals.**  Clipper prints `? 5` in
  ten columns (`         5`), a literal keeps its decimals (`1.5` prints
  `1.5`), and division takes SET DECIMALS (`10/4` prints `2.50`).  dBASE
  III does the same; this interpreter trimmed (`5`, `2.5`), wrong for both
  dialects.  **Done 2026-10-29**, in the interpreter and clip alike: a
  number carries its decimals and integer width (value_t ndec, nwidth);
  literals keep their written decimals, + and - the larger, * the sum,
  / and ** SET DECIMALS unless whole, fields their own width, a computed
  value (SQRT ...) SET DECIMALS; ? and ?? print it right-justified, SET
  FIXED ON with SET DECIMALS places (probe num-display).
- **BP-S1, STR() without a width.**  dBASE III gave ten columns and no
  decimals (`STR(1.5)` is `         2`); Clipper gives the number's own
  width and decimals (`       1.5`).  Taken Clipper's (2026-10-29); a
  width too small fills with asterisks, as Clipper does.

## The function batch (after step 9, 2026-10-29)

The 83 names the guide lists that were still missing, in
`dbase/src/clipfn.c`: the DB*() forms route through the commands they
stand for (DBSETFILTER builds SET FILTER TO from its text argument,
DBSETINDEX re-issues SET INDEX TO with the list, DBUSEAREA is SELECT 0
and USE ... ALIAS, DBCREATEIND is INDEX ON); DBCREATE builds the file
from the structure array; DBEVAL walks the scope; FIELDBLOCK /
FIELDWBLOCK / MEMVARBLOCK are set-get blocks over the reference; the
binary conversions, the memo-line family (MEMOLINE keeps a hard return
in its line, as MEMOTRAN expects), DESCEND, HARDCR, MEMOREAD / MEMOWRIT
(Ctrl-Z written and stripped), FERASE / FRENAME / DIRECTORY, ERRORLEVEL,
NEXTKEY (a one-key pushback INKEY honours), SETPRC, the flags SETCANCEL
/ SETBLINK / NOSNOW / SETMODE.  Probes fn-misc-batch and fn-db-batch
pass against Harbour.  Found on the way: an omitted argument (`f(a, ,
c)`) is NIL now; SELECT 0 picks the lowest free area; SET ORDER's
messages come under SET TALK like the rest.

Approximations, by design or by this implementation's limits:

- A string ends at a NUL: I2BIN / L2BIN give the bytes up to the first
  zero byte and BIN2x read the missing ones as zero, so a value with a
  zero byte between non-zero ones does not round-trip; MEMOREAD returns
  at most 255 characters (a value_t string).
- DBSETFILTER needs the condition's text (the block alone cannot be
  turned back into one); DBSETRELAT likewise.
- The interactive ones answer for a stream: ALERT prints the message and
  options and returns 1; ACHOICE shows the items and returns 1; BROWSE()
  and DBEDIT run the BROWSE command; MEMOEDIT returns its text
  unchanged.  SAVESCREEN is empty and RESTSCREEN paints nothing.
- DBSETDRIVER answers DBFNDX; DBUNLOCK / DBUNLOCKALL / NETERR / NETNAME /
  ISPRINTER / DOSERROR answer for a single user with no printer; SETMODE
  is true for 25 x 80 only.

## The standard headers (2026-10-29)

`clip/include/*.ch`: inkey.ch, set.ch, error.ch, getexit.ch,
setcurs.ch, box.ch, dbstruct.ch, directry.ch, fileio.ch, achoice.ch,
dbedit.ch, memoedit.ch -- this project's own text, the names and values
the language defines (checked against Harbour's).  `#include` looks in
the including file's directory, then `-I` directories, then the
compiler's include/ beside the s32-clip binary.  With them INKEY()'s
function keys took the standard codes (F1 28, F2 to F10 -1 to -9,
Shift-Tab 271), which dBASE III also used; the terminal reader keeps a
value of its own for "no key" now that -1 is a key.  SET() follows
set.ch's numbering and Clipper's types (a logical for the ON/OFF
settings, numbers, strings; the date format as "dd/mm/yyyy"; the value
before the change returned) -- it had a numbering of its own with "ON"
strings; the settings this interpreter does not act on (TYPEAHEAD,
SCOREBOARD, INTENSITY ...) are remembered so they round-trip.  A
command word written as a function with no blank before the
parenthesis -- SET(...), SELECT(...) -- is the function on both
engines (it was the command, and "Unknown SET option").  Not provided:
std.ch (the commands are built in), and 5.3's headers.

## The commands' clauses (2026-10-29)

Two probes against Harbour, each statement leaving a checkable mark:
`cmd-database-clauses` (USE NEW / ALIAS, the scopes and FOR / WHILE on
COUNT, SUM, AVERAGE, REPLACE, DELETE, RECALL, LOCATE and CONTINUE,
INDEX ON ... DESCENDING / FOR, SET INDEX TO a list and SET ORDER, PACK,
COPY TO with FIELDS / FOR / DELIMITED / SDF, COPY STRUCTURE, APPEND
FROM DELIMITED / SDF, ZAP, SORT /D, TOTAL ON ... FIELDS, SKIP ALIAS,
STORE TO a list, RELEASE ALL LIKE, CLOSE DATABASES) and
`cmd-program-clauses` (PRIVATE / PUBLIC / DECLARE with initializers,
FOR ... STEP, LOOP and EXIT, DO CASE / OTHERWISE, STORE, SAVE TO ALL
LIKE / RESTORE FROM ADDITIVE, TEXT / ENDTEXT, the three comment forms,
EXTERNAL / REQUEST).  Both pass.  Fixed on the way:

- USE takes NEW (the lowest free area), SHARED / EXCLUSIVE / READONLY /
  VIA, in any order before INDEX and ALIAS -- and closes the area the
  file goes into only once NEW has had its say (it closed the current
  one first, which emptied the alias NEW was meant to leave alone).
- **LIST and DISPLAY print Clipper's lines**: no heading, a new line
  first as ? writes, `Str(RecNo(), 7)` and a blank unless OFF, each value
  after a blank in its field's own width, a blank at the end.  dBASE
  III's headed listing is still there under SET HEADING ON (the default
  is OFF now).  A field list ends where its commas do (`LIST CODE, QTY
  OFF` took OFF for a field).  25 of the dBASE test expectations were
  re-recorded for this; the diffs were the heading and the widths only.
- AVERAGE's result carries SET DECIMALS; a CONTINUE that finds nothing
  leaves the pointer at EOF; STORE x TO a, b, c stores to all three;
  COPY TO ... SDF takes FIELDS after SDF as well as before; APPEND FROM
  ... SDF leaves blank the fields a short line does not reach (it read
  past the line's end into the previous record's bytes); the "Index
  opened / closed" and "Structure copied" messages come under SET TALK;
  TEXT lines are written as ? writes them (new line first); EXTERNAL,
  REQUEST and ANNOUNCE are accepted and do nothing.

Not probed yet: @ SAY / GET's clauses beyond PICTURE / VALID / WHEN /
RANGE, SET RELATION ADDITIVE, JOIN / UPDATE's forms, REPORT / LABEL
FORM, the console commands that need a terminal.

## CA's own sources (2026-10-29)

The sources CA shipped with Clipper 5.2 -- SOURCE/SAMPLE (forty small
library programs), SOURCE/SYS (the Get reader GETSYS.PRG, the default
error handler ERRORSYS.PRG, the report and label runners), DBU, PE and
RL -- are held beside the guides in `~/refs/clipper/source` (reference
material, CA's copyright, never into this tree).  They are the
compliance test probes cannot be: real Clipper code, written by the
people who wrote Clipper.

- **All 45 SAMPLE and SYS programs compile** with s32-clip (27 did at the
  first try).  What they needed: a load-time normalizer shared by both
  loaders (STATIC FUNCTION, FUNC / PROC, WHILE without DO, a bare END
  closing the innermost IF / DO WHILE / DO CASE / BEGIN SEQUENCE / FOR);
  FOR, WHILE and NEXT as variable names when = or := follows (DOT.PRG,
  Summer '87 style); `;` as a statement separator in a #command result;
  common.ch and assert.ch; 512 functions in a file; set.ch read for
  every program, as Clipper's std.ch included it; long identifiers kept
  whole (ten characters were significant: STACKISEMPTY and STACKISEMP
  were one name on one side of a lookup).
- **Drivers** (`tests/clipper/ca/ca-*.prg`, harness `run-ca.sh`): our own
  programs that call the CA functions with fixed inputs; driver and
  sources concatenated (the sources' file-wide preambles first, where
  Clipper wants file-wide STATICs), run under Harbour and under s32-clip,
  outputs diffed; SKIP without the sources.  Ten drivers pass: StrZero /
  Mod / LenNum, the time family, Date, String, Num, Stack and Dict,
  Iterator, Environ, Array, and ASRTDEMO as a whole program.  Found by
  them: a program's own function with a built-in's name replaces the
  built-in (CA's STRING.PRG defines PROPER, MOD.PRG MOD -- Clipper's
  linker took the program's symbol); EMPTY() of an array is its
  emptiness; SUBSTR() with a negative start counts from the end; `n % 0`
  is 0 (CA's MOD.PRG computes it before testing the divisor); YEAR() is
  five wide, MONTH() and DAY() three; TRANSFORM() of a number fills the
  template's digit positions from the right ("99/" of 3 is " 3/", too
  wide is asterisks, @( @B @C @X @Z @E); the blank date shows its
  separators ("  /  /  ").

- **The applications.**  DBU (eleven files, 6,000 lines, 152 functions)
  compiles to a SLOW-32 binary, starts, paints its key bar and menu
  (Help Open Create Save Browse Utility Move Set) and waits for a key.
  PE and RL compile.  What DBU needed: the command layer evaluating
  through the AST (FOR bounds, @ coordinates, DECLARE dimensions --
  expr.c's older evaluator knew no M-> or arrays), `;` continuations
  followed by a comment, `@ t,l,b,r BOX frame`, `@ r,c` alone, DECLARE
  with expression and zero dimensions, SET CURSOR, and tables that hold
  it (s32-clip's lines grow on demand; the runtime's function table held
  64 and dropped the rest without a word).

Not driven yet: GETSYS.PRG and ERRORSYS.PRG as replacements for the
built-in Get reader and error handler (they compile; running them in
place of ours needs the keyboard), the report and label runners
(need .frm / .lbl files), DBU past its first screen (keys).

## The order of work proposed

Each step closes probes and is gated by both oracles (Harbour for the
probes, the interpreter for the corpus and the user's own programs).
The severe ones are marked.

1. **Numeric display (BP-N1)** -- severe: the value representation, in
   the interpreter as well as compiled code; every printed number moves.
   Done 2026-10-29 (see BP-N1).
2. **The operators** (`:=`, `!=`, compound assignment, `++`/`--`) and
   IF(): the shared expression parser.  Done 2026-10-29: `:=` and the
   compound assignments and `++`/`--` (prefix and postfix) are expression
   nodes stored through shared helpers (an undeclared name is a memory
   variable, as the guide says; FIELD-> and alias-> assign fields, M->
   memory variables); `!=`, `%`; expression statements (x := e, x++,
   f(x) where f is no command); FOR i := ...; LOCAL / PRIVATE / PUBLIC
   with initializers; IF(), PCOUNT(); MEMVAR / FIELD as declarations.
   Found on the way: at run time / and % take SET DECIMALS even when the
   quotient is whole (24/4 from variables shows 6.00); two integer
   literals with a whole quotient fold to an integer at compile time
   (10/5 shows 2).
3. **Function calls as statements, formal parameters, PCOUNT(), `@`.**
   Done 2026-10-29 (`@` last): f(@x) passes x by reference -- the
   parameter's final value goes back to x when f returns, interpreted
   (the frame's with_ref_names, as DO ... WITH) and compiled alike.
4. **Literal arrays and the array functions.**  Done 2026-10-29: arrays
   are Clipper's -- one dimension, nested (DECLARE a[r,c] and a[i,j] are
   a[i][j]), resizable, shared by reference, not collected yet (a known
   debt: they live until the program ends); `{...}` literals, `[]` on
   any value, readable and assignable; ARRAY, AADD, ASIZE, ATAIL, AFILL,
   ACLONE, LEN(array), and the old ones on the new form (ASORT, AINS,
   ADEL, ACOPY return the array; the vacated element is NIL); LOCAL /
   PRIVATE / PUBLIC a[n]; assignment replaces an array (dBASE IV refused
   it without an index).  `{MM/DD/YY}` stays a date literal when the
   braces hold a date and nothing else.
5. **LOCAL and STATIC as Clipper has them** -- STATIC done 2026-10-29:
   one storage per variable (C static in compiled code, a table by
   program, line and name in the interpreter), initialized the first time
   only, bound as a LOCAL of each call and written back on return.
   LOCAL is still dynamic underneath (a memvar at the call's depth); the
   code-block depth rule covers what that changes.  Was: -- severe: lexical, not
   dynamic; compiled to C variables, which the interpreter's evaluator
   cannot see, so anything that reaches it (a macro) needs a bridge.
6. **Code blocks** -- severe: closures over LOCALs ("detached locals");
   a new value type, compiled blocks as C functions with an environment,
   macro-built blocks through the evaluator.  Done 2026-10-29: VAL_BLOCK
   (VALTYPE B), `{|p| e1, e2}`; EVAL, AEVAL, ASCAN with a block; the NIL
   literal (NIL equals only NIL).  A block runs at the call depth it was
   made at, so it sees its maker's LOCALs wherever it is EVALed; its
   parameters are bound as PRIVATEs are.  A detached local (the maker
   returned) is served from the block's own copy of the variables its
   body names, taken when it was made and kept up to date by the block --
   two blocks sharing one detached local do not see each other's changes
   (Clipper's do).  Compiled blocks are C functions.
7. **The preprocessor** -- done 2026-10-29, in s32-clip's loader (a
   Clipper program is compiled; the interpreter does not preprocess):
   #define (case-sensitive constants and NAME(a, b) pseudofunctions),
   #undef, #ifdef / #ifndef / #else / #endif, #include "file" (beside
   the including file), #error, #stdout, and #command / #xcommand /
   #translate / #xtranslate with literal words (any case) and `<name>`
   markers that take the text up to the next literal, `<"name">` and
   `#<name>` stringifying; lines expand until nothing changes.  `//` and
   `/* */` comments go first.  The full grammar followed the same day:
   optional clauses `[...]`, any number of them in any order; list
   `<x,...>`, restricted `<x:A,B>`, wild `<*x*>` and extended `<(x)>`
   match markers; the result forms `<x>`, `#<x>`, `<"x">`, smart `<(x)>`
   (a parenthesized or quoted argument as it is), blockify `<{x}>`,
   logify `<.x.>` and result groups `[...]` written only when a marker
   in them matched; the four-letter keyword abbreviation under #command
   / #translate, whole words under the x forms (probe dir-command-grammar,
   against Harbour's own expansions).  Not provided: STD.CH -- the
   built-in commands are parsed, not rules -- so a program's result text
   cannot run into them by abbreviation as it can in Clipper (`Display(`
   is the DISPLAY command there).
8. **The error system** -- done 2026-10-29, interpreter and clip alike:
   a value type for objects (VALTYPE O; shared by reference like arrays,
   not collected -- the same debt), the `:` send operator (`o:name` reads
   an exported variable, `o:name := e` assigns one, `o:name()` calls a
   method; `:className()` on any object), ERRORBLOCK() posting a handler
   block (returns the previous; NIL when none was), ERRORNEW(), the Error
   class with the guide's exported variables (args, canDefault, canRetry,
   canSubstitute, cargo, description, filename, genCode, operation,
   osCode, severity, subCode, subSystem, tries), BREAK with a value --
   statement and function form, so a handler can BREAK(e) -- and RECOVER
   USING var; a bare END closes a sequence.  An error makes an Error
   object and gives it to the handler; the handler BREAKs to the nearest
   RECOVER, or returns and the failed statement is abandoned; with no
   handler the sequence is recovered as before, RECOVER USING receiving
   the object.  Since the header batch (below) the object carries
   Clipper's genCode (error.ch's EG_*), description ("Argument error",
   "Zero divisor", "Bound error", "Variable does not exist" ...), the
   operation ("+", the variable's name, "array access") and the BASE
   subCode (1081 for +, 1003, 1132 ...) -- probe fn-error-codes matches
   Harbour on all of them; an error this mapping does not know keeps its
   own text as the description.  Still not Clipper's: args is NIL;
   canRetry / canSubstitute are always false, so a handler's return value
   substitutes nothing; ERRORBLOCK() with none posted returns NIL where
   Clipper returns the default handler's block.
9. **The Get system, TBrowse** -- done 2026-10-29, interpreter and clip
   alike (dbase/src/getsys.c).  *Get:* GETNEW(row, col, block, name,
   picture, color) and the class's variables (buffer, changed, original,
   pos, hasFocus, type, exitState, cargo, pre/postBlock, decPos, minus,
   typeOut, badDate, rejected ...), varGet / varPut through the block or
   the variable's name, setFocus / killFocus, assign, undo, reset,
   updateBuffer, unTransform, display, and the editing methods
   (overstrike, insert, delete, backSpace, home, end, left, right,
   toDecPos) on the buffer under the picture (@! upper, templates,
   numeric widths).  `@ r,c GET var` makes a Get beside the GET it
   records (VALID and WHEN as postBlock / preBlock) and AADDs it to a
   GETLIST array when the program keeps one; READ and READMODAL() run
   dbase's editor over the GETs recorded; READVAR(), UPDATED(),
   GETACTIVE() answer during and after it.  *TBrowse / TBColumn:*
   TBROWSENEW, TBROWSEDB (the three blocks on the work area through
   DBGOTOP / DBGOBOTTOM / DBSKIPPER, added with DBSKIP), TBCOLUMNNEW;
   add / ins / del / get / setColumn, goTop, goBottom, up, down, pageUp,
   pageDown, home, end, left, right, pan*, stabilize, refreshAll /
   refreshCurrent / invalidate, configure, hilite / deHilite; rowCount
   from the window less heading, headSep, footing rows; hitTop /
   hitBottom settled at stabilize() over the movement requested since
   the last one, as Clipper's deferred movement makes them (three up()
   from row 3 hit nothing); a column's width from its own or the longer
   of heading and the current cell.  stabilize() paints the whole window
   at once, with a terminal only (Clipper's stream output drops
   positioned text too; a browse repainted into a line stream says
   nothing); the current row is not highlighted.  *Screen functions:*
   SETPOS / DEVPOS, DISPOUT / DEVOUT, MAXROW, MAXCOL, SETCOLOR (returns
   the spec), SETCURSOR, SCROLL (clears the region), DISPBOX, SAVESCREEN /
   RESTSCREEN (no screen memory: empty, and nothing), SETKEY with a block
   (procname, line, readvar), OUTSTD, OUTERR, TONE, DISPBEGIN / DISPEND,
   ISCOLOR.  Also from this step: IIF() evaluates one branch only (both
   were, so `{|x| IF(x == NIL, v, v := x)}` cleared v); a file whose top
   level is declarations only runs its first procedure, as Clipper does;
   USE positions at the first record (it left none, and a field read
   before any movement failed as an unknown variable).  Not Clipper's
   yet: READMODAL() reads the GETs recorded by @ GET, not the Gets in the
   array it is given; the Get's reader and exitState are inert; a
   column's colorBlock, footing painting, TBrowse freeze and horizontal
   panning are accepted and ignored; NEXTKEY, KEYBOARD and SETMODE are
   absent.
