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

- **DBU driven with keys** (later the same day).  Keys go in through
  the terminal service as the dbase INKEY tests feed them (bytes on
  stdin with short pauses; `scratchpad/vt.py` renders the escape stream
  as the final 25x80 screen).  Esc then Y leaves DBU cleanly; F2 Open,
  Enter (Database), Enter on PEOPLE.DBF in the file pane, Enter on Ok
  opens the table and shows it under Files; F5 Browse, Enter paints the
  records through TBrowse and the arrows move the highlight (Record
  3/5).  What that took, each found by tracing the compiled program
  (`CLIP_TRACE=1` prints every statement and every command handed to
  the interpreter on stderr):
  - **ACHOICE()** was a stub (print the items, answer 1): DBU's menus run
    on it, so the main screen spun calling it.  Now the guide's two key
    sets (no user function: Home / End / Return / Esc / side arrows /
    first letters are its own; with one, only vertical movement is, and
    every other key is an exception the function answers), scrolling,
    the parallel selectable array (logicals or macro strings), the
    initial item and window row, AC_IDLE / HITTOP / HITBOTTOM / EXCEPT /
    NOITEM and the answers ABORT / SELECT / CONT / GOTO, Harbour's
    calling sequence exactly (one user-function call per pass; GOTO
    replays the key as a first-letter search).  Probe fn-achoice.  The
    one point where Harbour departs from the guide (an unselectable
    initial item stays) is tests/achoice.prg, under the interpreter
    oracle, with the guide's rule.
  - **KEYBOARD** and **CLEAR TYPEAHEAD** (a typeahead buffer read before
    the terminal by every wait state; each byte one key 0..255, so
    CHR(252) is 252 -- Harbour's answer, and DBU's menu_key() expects
    exactly that); SET TYPEAHEAD accepted.  The key codes live in one
    header, `dbase/src/keys.h` (screen.c, menu.c and browse.c each had a
    copy; browse's Shift-Tab was 256, the others' 271).
  - **Macros through the AST.**  An expression containing `&` never
    reached the AST evaluator (its cache refused the text) and fell to
    expr.c's old one, which knows no M->, arrays or blocks: DBU's
    `&el[M->page]` failed, `&el[M->page] = 1` was an unrecognized
    command, and the error loop filled a 64 MB heap in two seconds (so
    the first symptom was "Out of memory").  Macro texts are compiled
    afresh each time, uncached; a statement that begins with a macro is
    expanded and executed; **`&(expr)`** is a node of its own, the
    string compiled when evaluated (DBU builds its column blocks that
    way).  Probe dir-macro-forms.
  - **SELECT (expr)**, `M->name = value` / `alias->field := value` as
    statements, **USE (expr) ALIAS (expr)** with the clauses in any order
    (DBU's NetUse), and **ADIR()** filling its arrays (names, sizes,
    dates, times, attributes -- it only counted, so DIRECTORY() was
    empty too and the file pane blank).
  - The command table is binary-searched: an entry added out of order
    made DO vanish.  It sorts itself on first use now.
  - The compiled program's stack is 1 MB (`STACK_SIZE` overrides): DBU's
    main screen overflowed 128 KB.
  - **SAVESCREEN() / RESTSCREEN()** are real: the terminal's cells live in
    the host, so screen.c keeps a shadow of the screen (every write dbase
    makes goes through its wrappers; menu.c and browse.c no longer touch
    the terminal directly) and a saved region is handed out as a short
    handle (a string value holds 255 characters, a screen is 4,000
    cells).  Leaving Browse puts DBU's view back; the Set menu drops
    down over it afterwards.
  - A `;` continuation joins the next line with its indentation reduced
    to one blank (it was none: `22` then `.AND.` on the next line became
    the number `22.`), and the lexer ends a number before `.AND.` /
    `.OR.` / `.NOT.` / `.T.` / `.F.`, as Harbour's does.  Probe
    dir-continuation.
  - TYPE() evaluates its text (it answered "C" for every string, so
    DBU's "is the filter logical?" could never be true): C N D L A B O,
    U for an undefined name, UE for text that does not compile.  Probe
    fn-type.  AFIELDS() fills its four parallel arrays; TBrowse answers
    forceStable.
  - An ACHOICE() window is placed by the initial item as given, even when
    the highlight moves on to the first selectable one (DBU's Set menu
    starts on an unselectable Relation that must stay in view).
  - **Scope depth under a block.**  A block is evaluated at its maker's
    depth (so the maker's LOCALs are visible); a function called from
    inside it then took maker + 1 -- a live frame's own depth -- and
    leaving released that frame's LOCALs.  DBU's freshOrder() lost its
    nRec across oB:forceStable(), whose column blocks call functions.  A
    call now takes a depth above every live frame (`live_max` in the
    store, saved and restored by the compiled scope).  Probe
    dir-block-locals, Harbour agreeing, detached locals included.
  - **The GET editor** has Clipper's keys (Left / Right / Home / End,
    Ins toggling insert, Del, Backspace, Ctrl-Y, Ctrl-U, Up / Shift-Tab
    to the GET before, Down / Tab on, PgUp / PgDn / Ctrl-W ending the
    READ, Esc) and the picture's functions (@K clear on the first key,
    @S<n> a scrolling window, @!, @A, @R) apart from its template, whose
    length is the field's width -- the whole string's was, so "@KS30"
    made a field four wide.  `@ r, c GET M->var` names the variable
    (the recorded name was "M"), `PICTURE M->pfunc` is an expression,
    and READ, MENU TO and BROWSE read their keys through the typeahead
    buffer, so the key DBU's dialog re-stuffs with KEYBOARD reaches the
    field.  With those, Set / Filter takes "AGE > 40" and Browse shows
    the three records under DBU's "<Filter>" mark.
  - **Editing a Browse cell** (Enter on it, type, Enter) took four more
    rules, each with a Harbour probe: PCOUNT() inside a block counts the
    block's own arguments (CA's set/get idiom `{|x| if(PCount() == 0,
    var, var := x)}`; probe dir-block-pcount); `.AND.` and `.OR.` leave
    the right side alone when the left decides (DBU tests a Get's type
    before LEN() of its value; probe dir-short-circuit); a Get over a
    set/get block reads and writes through it (probe fn-get-setget); and
    READMODAL() reads the Gets in its array, each shown with its value
    and stored back through its block (it read the GETs recorded by @
    GET).  ROW() and COL() answer the shadow's cursor under a terminal.
    Ada's city becomes Paris, on screen and in the file.
  - **CREATE** in a program is Clipper's: `CREATE <file>` makes the
    structure-extended file (FIELD_NAME, FIELD_TYPE, FIELD_LEN,
    FIELD_DEC) and USEs it, `CREATE <file> FROM <ext> [ALIAS a] [NEW]`
    builds the file it describes; the dot prompt keeps dBASE III's
    dialogue.  A name that has an extension keeps it (DBU's ddbbuuuu.ext
    became DDBBUUUU.EXT.DBF, and the Open dialog then listed it).  F3
    Create paints DBU's structure editor.
  - **Move and Utility.**  Inside Browse, F7 Goto 4 lands on record 4,
    Locate NAME = "Grace" on record 2, Skip -1 back one; Utility / Copy
    writes a second table with the five records; the Replace dialog
    paints its field list.  Needed: GO and SKIP taking expressions
    (DBU's `goto nWhich`, `skip -nStep`; they read a number only); a GET
    shown at @ GET time cut to its picture's window (a 127-wide value
    painted whole ran over the dialog's frame); TBrowse hilite() and
    deHilite() painting the current cell and parking the cursor on it,
    and a stabilize() with nothing left to paint parking it too (DBU
    writes its status line, calls stabilize() again, then makes its
    editing GET at ROW(), COL()).
  - TYPE() took dBASE III's semantics in three of the dBASE tests (the
    type of the string argument itself); re-recorded to Clipper's, which
    dBASE III shares for a string argument; a non-string argument still
    answers its own type, as those tests have it.

- **ERRORSYS.PRG as the error handler** (driver ca-errorsys): CA's
  ErrorSys() posts its DefError() over ours; its own `#command ? ... =>
  ?? Chr(13) + Chr(10) ; ?? <list>` and `?? <list,...> => OutErr(<list>)`
  take over output, and a zero divisor, which DefError() answers with 0,
  gives the expression 0 (Harbour: the same).  Two things it took: a
  rule's result holding several statements (`;` between them) is split
  before any later rule sees it (the `??` rule had taken "Chr(13) +
  Chr(10) ; ?? cMessage" as its list); and an error the operation can
  take a substitute for runs the handler at the point of failure, with
  canSubstitute set, and uses a value it returns (the handler ran only
  at statement level before, after the expression was already lost).
  Probe fn-error-substitute.  Its dialogue path (Alert() with Quit /
  Retry, then "Called from" lines) is not driven: Harbour's CGI
  terminal renders Alert() at exit, in its own way.

- **GETSYS.PRG as the Get reader.**  DBU and CA's GETSYS.PRG compiled as
  one program (the scratch build; no harness, it needs the keyboard):
  DBU's Filter dialog takes "AGE > 40" and a Browse cell takes its new
  value through CA's ReadModal() / GetReader() / GetApplyKey() over our
  Get class, Browse showing the result either way.  What it took:
  Clipper's compiler gives every function a LOCAL GetList := {} of its
  own, so `@ GET` makes one at the current depth when none is visible;
  READ runs ReadModal(GetList) when the program supplies ReadModal()
  (the override rule: a program's function replaces the built-in),
  then clears the list and the recorded GETs; SET() acts on its
  settings directly (it went through the functions by name, and
  GETSYS.PRG's own ReadInsert() calls Set(_SET_INSERT): a loop until
  the stack ran out); SET SCOREBOARD is kept, since GETSYS paints Ins
  at row 0, column 60 unless it is off (DBU's menu bar lost "Mo").  The
  Get class gained the word moves and deletions GETSYS binds (probe
  cls-get-words, Harbour's rules).

- **The report runner, and the wall it found.**  A driver over CA's
  FRMRUN.PRG and FRMBACK.PRG with a form our CREATE REPORT wrote.  First
  the form: Clipper's reader (dBASE III's layout, FRMDEF.CH) wants
  0-based expression pointers with 65535 for none, lengths that count
  the NUL ending each text, the column's totals as a Y/N byte at offset
  5 with decimals at 6, Y/N bytes for spacing and summary, and -- "dBASE
  skips first 12 byte fields block" -- the columns from the second
  12-byte slot.  Ours had 1-based pointers, no NULs, the words in other
  places and the columns one slot early; Harbour read every expression a
  character short and the columns one off.  Writer and reader now follow
  Clipper's, the dBASE suite agreeing with itself as before, and Harbour
  prints the whole report over our form (the heading, the column
  headings, the detail lines, the total; the summary form).  Our runtime
  cannot run it: FRMBACK reads the 1,990-byte form with one FREAD into
  one string, and a string value here holds 255 characters and ends at
  the first NUL.  That is the string-capacity debt, with its first hard
  case; the driver, the form and Harbour's output wait in
  `tests/clipper/ca/blocked/`.  FREAD has Clipper's by-reference form
  for reads that fit (probe fn-fread-ref).

- **Strings of any length, NULs inside** (2026-10-30).  A character
  value keeps its 255-character inline array and gains a heap buffer and
  a length beside it; `val_text()` and `val_len()` see the whole value,
  `val_strn()` makes one, and every character value is made by one of
  them, so the fields are always set.  The operators (+ == = < $) and the
  string functions (LEN SUBSTR LEFT RIGHT AT RAT CHR ASC SPACE REPLICATE
  TRIM LTRIM ALLTRIM UPPER LOWER PADL PADR PADC STUFF STRTRAN EMPTY), the
  binary conversions (BIN2x / x2BIN round-trip through zero bytes now),
  FREAD / FREADSTR / FWRITE / MEMOREAD / MEMOWRIT, the memo-line family
  and both `?` paths work on the whole text; a reader not yet converted
  sees the first 255 characters.  Probe fn-long-strings, Harbour
  agreeing on every line; the one dBASE test that recorded the 255 cut
  re-recorded.  With it, **CA's REPORT FORM runner** (FRMRUN.PRG,
  FRMBACK.PRG) prints the whole report over a form our CREATE REPORT
  wrote, byte for byte with Harbour (driver ca-frmrun, out of blocked/);
  the last thing it needed was `a[i, j, k]`, three subscripts in one
  bracket, which the parser stopped at two.  **CA's LABEL FORM runner** (LBLRUN.PRG, LBLBACK.PRG) prints the
  labels over a form our CREATE LABEL wrote, the same way (driver
  ca-lblrun; the .lbl layout already matched).  A driver is a
  PROCEDURE Main of its own now: as an implicit main after the hoisted
  preambles, Harbour took the sources' file-wide STATICs for the main's
  own.

- **Memory is collected** (2026-10-30, `dbase/src/gc.c`).  Nothing a
  program made was ever freed: arrays, blocks, objects, and now long
  strings' buffers.  A collector with conservative roots and precise
  contents: every such allocation is registered; at a statement boundary
  (compiled `clip_at`, interpreted `prog_execute_line`), once 4 MB have
  been allocated since the last round, the data and bss segments and
  the C stack (registers spilled by setjmp first) are scanned for
  registered addresses, what they name is marked, marking follows the
  values inside arrays, blocks and objects, and the rest is freed.  No
  root list to maintain: the variable store, the compiled scopes, the
  interpreter's frames, a built-in's arguments and a compiled
  statement's temporaries are all in those segments or on that stack.
  The one holder neither scan sees, a heap buffer of values an
  evaluator owns for a moment (its argument arrays), is pushed as a
  root while it lives; a parser's literal text is pinned with its tree.
  The explicit frees the variable store did on release are gone (they
  freed an array a second variable still shared).  A SLOW-32 build
  collects; a host build registers and keeps everything, as before.
  Checked by stress programs: 200,000 iterations making five values each
  run in the 64 MB heap collecting ~1,300 at a time, with values kept
  across collections intact; temporaries held across a call that
  collects (an array literal waiting as an argument while the callee
  allocates past the threshold) survive in both the interpreter and
  compiled code.  `DBASE_GC_TRACE=1` prints each round.

- **Utility's Pack, Zap and Copy** run from keys ("PEOPLE.DBF Packed",
  "Zapped", a copy with the five records); the Append dialog opens and
  validates (its "cannot be appended from itself" when fed its own
  file).  Found on the way: `SET KEY -(M->i) TO clear_gets` -- our
  SET KEY took a literal number only, so every F-key handler DBU
  installs around a READ landed on key 0 and an F-key pressed inside a
  GET went nowhere; the key is an expression now.

- **The Create flow, end to end** (2026-10-30): F3 / Database, two
  fields typed, F4 / Struct, a name, Ok -- NEWF.DBF with NAME C 20 and
  AGE N 3, opened in the view.  Three things stood in the way, none of
  them DBU's.  A GET on a field stored into the record and also into a
  memvar of the field's name, so the next GET on that field found the
  memvar first (the second field showed "NAME" and kept nothing); the
  store goes to the record only, and a VALID refusal puts the shown
  value back.  `CREATE &filename FROM ddbbuuuu.ext ALIAS cAlias` lost
  everything after the macro: the lexer's rest-of-line began inside the
  macro's expansion, and it begins at the macro in the input now
  (`token_src`), so FROM and ALIAS are read -- the alias is CALIAS, as
  Clipper's smart stringify makes it, and Harbour agrees (probe
  cmd-create-from-macro).  ERASE said "File erased." whatever SET TALK
  said; it is TALK's now, so a Clipper program's ERASE is silent.  Not
  a bug but an hour: Enter on a Character field's width stays on that
  item (DBU's own `l_usr` table), so a key script that typed on expecting
  to have moved re-entered the width GET and left 0 there.  Also from
  this round: Alert() under a terminal is a boxed dialogue (message
  split at `;`, options with the current one in colour pair 1,
  Left/Right/Tab/Space/Enter/Esc and first letters, screen restored,
  the option's number returned); a numeric GET sits right-justified in
  its field and the first digit typed replaces it (20 over 10 became
  200).  `DBASE_KEY_TRACE=1` prints what INKEY() sees, beside
  `CLIP_TRACE`.

- **Append, and the view's panes** (2026-10-30, later).  Utility /
  Append from a second file: "Append completed", seven records.  The
  keys were the lesson: a DBU dialog opens with its file list focused,
  Enter takes the highlighted file and moves on, and a letter typed on
  the dialog's elements is a first-letter jump between them (so "O",
  Enter reaches Ok) -- the File element is typed into only after Enter
  on it.  The view's Fields pane was blank after Open: DBU lists it
  with `ACHOICE(..., .F., "", top_el)`, nothing selectable and no user
  function, which Clipper paints and returns from at once; ours
  returned without painting.  Also: the five SET options DOT.PRG
  resets at its prompt that we called unknown -- DECIMAL (the
  abbreviation), DEFAULT TO, DELIMITERS ON/OFF/TO, EXCLUSIVE, FORMAT TO
  -- are accepted and kept (probe cmd-set-kept); the box ALERT() and
  `@ ... TO` draw has the terminal's line glyphs instead of `+-|`.
  CA's DOT.PRG (the dot-prompt sample, dated 1986) is not a driver:
  it declares `PROCEDURE at` and then calls `at(...)`; Harbour refuses
  the file ("Redefinition of predefined function AT"), and with us the
  procedure shadows the function, as a linker override would.

- **RL, the report and label designer** (2026-10-30).  CA's RL
  (RLFRONT, RLDIALG, RLBACK concatenated) runs from keys: the Report /
  Label / Quit menu, the file dialog, the column editor with two
  columns typed in, F10, "Save Changes To File TEST?", Ok -- and the
  TEST.FRM it writes prints the same report under Harbour's REPORT FORM
  and ours.  Eleven things stood in the way, none of them RL's:
  - every cursor move was one row and one column off: the terminal
    service counts from 1 and screen.c sent 0-based positions, so row 0
    and row 1 landed together (DBU's and RL's top lines overlapped
    without anyone noticing, since both start at row 0);
  - `@ ... PROMPT` took only a quoted literal (RL's is
    `PROMPT aMainMenu[1]`), and `@ r,c TO r,c2` on one row drew a box's
    bottom edge instead of a line;
  - `REPLACE alias->field WITH ...` was "Field not found" (RL edits its
    work file as `form_dbf->contents`); it writes the other area's
    current record now;
  - `@ ... GET a[i]` and `@ ... GET alias->field` were read as a
    variable named `a`; they get the block Clipper's preprocessor makes,
    `{|x| IIF(PCOUNT() == 0, a[i], a[i] := x)}`, and the editor stores
    through it;
  - `@ r,c SAY ... GET ...` handed the GET to the SAY's expression and
    took its PICTURE as the SAY's; the GET follows the text now;
  - READ never emptied GetList, so RL's editor loop piled eight Gets a
    pass into it until the heap ran out (the collector could free
    nothing still referenced); READ releases them as Clipper's
    `GetList := {}` does;
  - a character field over 255 wide (RL's HEADER C 260) keeps its high
    byte in the decimals slot, as Clipper writes it; we read 4;
  - SAVE took only `SAVE TO f ALL LIKE s` (RL writes
    `SAVE ALL LIKE frm_* TO &mem_file`), RESTORE only `RESTORE FROM f
    ADDITIVE` (RL also writes `RESTORE ADDITIVE FROM`), and the .MEM was
    our own layout; it is dBASE III's / Clipper's now (32-byte heads,
    name, type with the high bit, length; 0x1A at the end) -- probe
    cmd-save-restore;
  - a memory variable's name is ten characters significant, as
    Clipper's identifiers are: the .MEM holds FRM_PAGEHD, and RL reads
    `frm_pagehdr` after restoring it.  Harbour keeps long names distinct,
    so here we follow Clipper over the oracle;
  - a SET KEY handler's CLEAR GETS ended nothing: the READ went on with
    no Gets (RL's F10 is `CLEAR GETS; exit_status := .T.`);
  - CLOSE DATABASES closed the FOPEN/FCREATE handles too, and RL closes
    its databases between writing a report's columns and its last 24
    bytes; the guide's CLOSE is about work areas.
  The s32-clip statement limit went from 511 to 2047 characters (RL's
  PRIVATE list joined from nine lines was cut, and the rest ran as a
  command); a longer one is reported, not dropped silently.
  Our own REPORT FORM, over the form RL wrote, differs from Harbour's in
  layout: no leading blank line, the page number one column wider, the
  heading's blank lines and the totals line placed differently.  CA's
  FRMRUN (driver ca-frmrun) matches Harbour; the built-in one prints
  dBASE III's layout, which the dbase tests record.

- **REPORT FORM in Clipper's layout** (2026-10-30).  A compiled Clipper
  program's runtime now prints CA-Clipper 5.2's REPORT FORM layout
  (report.c `report_generate_clipper`, switched on in clip_init); the
  interpreter keeps dBASE III's, which the dbase tests record.  The
  rules are those of CA's FRMRUN as CA built it, with S87_COMPAT
  defined: an empty line before the first page, "Page No." + STR(n,6),
  the date, the title lines centred, an empty line, the column
  headings (numeric ones right-aligned, a trailing `;` adding a line),
  two empty lines; character columns wrapped to their width through
  MEMOLINE with the text trimmed before counting; group and subgroup
  headers, "** Subtotal **" / "* Subsubtotal *" and "*** Total ***"
  above their lines of totals; form feeds only to a printer; a PLAIN
  report still prints its first header and counts that page's lines.
  The .frm reader keeps three option bits it dropped (plain page, no
  eject before, eject after each group).  Checked three ways: Harbour on
  plain, summary, NOEJECT, HEADING and FOR variants and on grouped
  forms (probe cmd-report-form, which writes its own .frm); and CA's
  FRMRUN compiled by s32-clip under our runtime over grouped and
  double-spaced forms, line for line.  Harbour departs from CA's code
  in one place: a double-spaced record at a page's last line goes to
  the next page in Harbour and stays in CA's; we follow CA.
  On the way: TRANSFORM() on characters, dates and logicals took no
  picture functions at all; it has the guide's (@! @R @S<n> @D @E, the
  template's A N X 9 # ! and inserted or overwriting literals, L and Y
  for logicals; @L and @Y are GET functions and ignored, as Harbour
  ignores them) -- probe fn-transform-pictures.  MLCOUNT("") is 0, and
  MEMOLINE() past the last line is blanks the width wide -- probe
  fn-memoline-edges.  The runtime's command buffer is 2047 characters,
  matching s32-clip's statements.

- **RL's Label side, DBEDIT, LABEL FORM** (2026-10-30).  RL's label
  designer runs from keys: Label, a new file, the dimensions READ, F2
  into the lines, two lines of contents typed, F10, Ok -- and the .LBL
  it writes prints the same labels under Harbour's LABEL FORM and ours.
  What it needed:
  - DBEDIT() was a stub that ran BROWSE.  It is the guide's now, built
    on our TBrowse (getsys.c): the columns from the array or every
    field, pictures and headings, the double-line heading separator, the
    cursor keys, the user function called once with DE_IDLE /
    DE_HITTOP / DE_HITBOTTOM / DE_EMPTY when no key waits and with
    DE_EXCEPT for every other key, its 0/1/2 answers obeyed;
  - `@ ROW(), COL() GET &cur_field`: a macro GET target is expanded
    (the GET was registered on nothing and the line kept no text);
  - INKEY() did not make a function key LASTKEY() -- only positive codes
    were kept, so RL's `CASE LASTKEY() = -9` (F10) never matched;
  - a numeric GET is shown right-justified in its picture from the
    start ("  5", not "5  ").
  LABEL FORM in a compiled program prints CA-Clipper's layout (label.c
  `label_generate_clipper`, under the same switch as REPORT FORM): a
  label line whose value is blank is dropped and the label closes up,
  a line with no expression stays as space, each band line is trimmed,
  a last partial band prints as it stands -- CA's LBLRUN's rules.
  Probe cmd-label-form writes its own .lbl (two across, gap, margin,
  lines between, a blank value, an empty line, FOR) and agrees with
  Harbour.  SAMPLE still takes the dBASE path; Harbour's waits for
  "Do you want more samples?" and is not probed.

- **ERRORSYS and GETSYS under a terminal** (2026-10-30).  Both of CA's
  dialogue paths run from keys, and the built-in reader behaves as
  GETSYS does.
  ERRORSYS: every error but a zero divide reaches Alert() -- Quit, and
  Retry / Default when the error allows them.  What it needed: a
  program's ErrorSys() is run at startup, as Clipper runs it (s32-clip
  emits clip_startup() after the functions are registered), so its
  error block is posted; a failed USE raises Clipper's open error
  (DBFNTX/1001 "Open error" on the file, DOS error 2, canRetry and
  canDefault), and the handler's .T. opens the file again
  (prog_error_detail / prog_error_retry, for any command to use); Quit
  prints the message and "Called from MAIN(9)" and exits with
  ERRORLEVEL() -- QUIT and the end of a program now exit with it
  (they gave 0).  PROCNAME()/PROCLINE() now count a code block being
  evaluated as a level, "(b)NAME" on the line it is written on
  (compiled blocks push a frame; RECOVER drops the frames a BREAK
  left), and report nothing below Main when the program's top level is
  declarations only -- probe fn-procname-levels.
  GETSYS: Clipper's READ is GETSYS, so in a compiled program the
  built-in reader does what it does: with SET SCOREBOARD on, row 0
  from column 60 shows "Ins" while inserting, "Invalid Date" for a date
  that is not one and "Range: lo - hi" for an edited value out of its
  RANGE; a message stays until a key waits (not taken) and is wiped; a
  VALID refusal says nothing; a refused entry is edited again as typed,
  the cursor home.  On the way, in both readers: a date GET edits
  through its date format with the separators fixed, and a date or
  logical GET stores a date or a logical (both stored strings); a
  template's literals stand in the field; a string GET is its own
  length wide (it was ten at least); an accepted number shows through
  its picture (" 50"); the last character before an auto-advance is
  painted; end of input after a refusal ends the READ instead of
  re-editing forever.  CA's own GETSYS.PRG now reads the same GETs to
  the same results: the Get class sets `clear` in setFocus() for a
  number or @K and the first key blanks the buffer; typing steps over
  template literals; a focused Get repaints itself and parks the
  cursor after each message; killFocus() shows the variable again;
  badDate() is a method judged on the buffer (an empty date is not
  bad); reset() keeps `changed` (GETSYS's RangeCheck asks it after the
  reset); a variable's Get reads and assigns through Clipper's block,
  so GETSYS reaches a LOCAL; RANGE becomes VALID
  {|_1| RangeCheck(_1,, lo, hi)} when the program reads, and
  RANGECHECK() is built in (GETSYS's own replaces it).
  The date functions on an empty date give 0, "" and blanks (YEAR()
  gave -4713), DOW() is three wide -- probe fn-empty-date.

Not driven yet: nothing from the list.  Retry exists for USE only; the
other commands' errors offer Quit (and Default where they did).  A
terminal oracle for GETSYS would need Harbour driven through a pty,
which the probe harness does not do.

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
   panning are accepted and ignored; SETMODE is absent (NEXTKEY and
   KEYBOARD came with the DBU round below).
