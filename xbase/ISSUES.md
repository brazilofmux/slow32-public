# dBase III Clone: Open Issues & Future Work

This is a *living* planning document. Completed items are summarized at the end.

## 1. Open Bugs (Correctness)

### 1.1 ~~CALCULATE expression capture breaks with macro expansion~~ — RESOLVED
Fixed: expression text is now captured by scanning the raw original input string
(paren-balanced from `token_start+1` to matching `)`) instead of using pointer
arithmetic on `token_start`/`l->p`, which break when the multi-character operator
lookahead in `lex_next` triggers macro expansion as a side effect.
The callback also uses `ast_eval_dynamic()` for consistent macro re-expansion.

### 1.2 ~~String Concatenation Drop~~ — RESOLVED
Fixed: string concatenation (`+` and `-`) now truncates to fill the 255-byte
buffer instead of dropping the right operand entirely.

### 1.3 ~~Hardcoded Date in dbf_create~~ — RESOLVED
Fixed: `dbf_create()` now uses `time()`/`localtime()` for the file header date.

## 2. Behavior Regressions / Architecture Follow-ups

These are potential behavior shifts introduced by AST-based evaluation and
lexer changes. Confirm against dBase III expectations.

### 2.1 ~~Macro expansion timing~~ — RESOLVED
Expressions containing `&` now skip AST caching and recompile fresh on every
evaluation via `ast_eval_dynamic()`, matching dBase III/FoxPro semantics.

### 2.2 Array access resolution time — WON'T FIX
`name(...)` becomes array access *only if* the array exists at compile time.
If the array is declared later, the AST remains a function call.
This matches dBase III behavior, which resolves array references at compile time.

### 2.3 ~~AST function-arg parsing limits~~ — RESOLVED
`ast_parse_primary()` now enforces `MAX_FUNC_ARGS` at compile time and handles
`realloc` failure with proper cleanup and "Out of memory" error.

### 2.4 REPLACE Performance & Parsing — RESOLVED
Refactored `cmd_replace()` to parse field/expression pairs once before the record
loop. Expressions pre-compiled to AST (unless they contain `&` macros). Evaluation
inside the loop uses `ast_eval` or `ast_eval_dynamic`. FOR/WHILE clauses also
pre-compiled via `clause_compile()`.

### 2.5 REPLACE Index Inconsistency — RESOLVED
Refactored `cmd_replace()` to use `process_records()` with a `replace_cb`
callback when an explicit scope or FOR/WHILE clause is present. This makes
REPLACE traverse via `controlling_index()` like `COUNT`, `SUM`, `DELETE`, etc.
The default case (implicit NEXT 1, no conditions) still replaces the current
physical record directly, since after `APPEND BLANK` the index cursor and
physical cursor may diverge.

### 2.6 Recursive prog_run — RESOLVED
Wrapped the main while loop in `for(;;)` and replaced the recursive tail call
after `pop_frame()` with `continue`. Deep DO call chains no longer accumulate
host C stack frames.

## 3. Polish & Completeness (Open)

### 3.1 ~~Error handling completeness~~ — MOSTLY RESOLVED
~40 runtime error sites converted from bare `printf()` to `prog_error()` /
`prog_error_fmt()`, routing through the ON ERROR handler. Added standard
dBase III error codes: ERR_UNIQUE (143), ERR_CANNOT_CREATE (110),
ERR_CANNOT_OPEN (111), ERR_ALIAS_NOT_FOUND (128), ERR_TOO_MANY_VARS (18),
ERR_OUT_OF_MEMORY (22), ERR_STACK_OVERFLOW (24). Fixed ON ERROR handler to
run synchronously via `prog_call_sync()`. UNIQUE violations, file I/O
failures, record-range errors, and division by zero are all now catchable.
Remaining: syntax/parsing errors, interactive-mode restrictions, SET
validation, and menu/popup definition errors still use direct `printf()`
(these are not catchable in real dBase III either).

### 3.2 ~~LIST STRUCTURE / COPY STRUCTURE EXTENDED~~ — RESOLVED
`LIST STRUCTURE` now works (same output as `DISPLAY STRUCTURE`).
`COPY STRUCTURE EXTENDED TO <filename>` creates a DBF with fields
FIELD_NAME (C,10), FIELD_TYPE (C,1), FIELD_LEN (N,3,0), FIELD_DEC (N,3,0),
one record per source field. Used for dynamic schema operations.

### 3.3 ~~SAVE SCREEN / RESTORE SCREEN~~ — RESOLVED
Implemented via host-side screen buffer stack in the term service. Two new
term opcodes (`SAVE_SCREEN`, `RESTORE_SCREEN`) push/pop screen snapshots
in the emulator. The emulator maintains a shadow buffer tracking all
`PUTC`/`PUTS`/`CLEAR`/`MOVE_CURSOR`/`SET_ATTR`/`SET_COLOR` operations.
Supports up to 8 levels of nesting. Restore repaints via ANSI escapes with
minimal redundancy (skips default-attribute spaces, batches attribute changes).

### 3.4 ~~REPLICATE Off-by-one~~ — RESOLVED
Fixed: `fn_replicate` now allows up to 255 characters (was 254 due to
off-by-one in buffer size check).

### 3.5 ~~dbf_memo_read Wasteful I/O~~ — RESOLVED
Fixed: `dbf_memo_read()` now stops reading once the destination buffer is
full instead of continuing to scan for the 0x1A terminator.

### 3.6 ~~Large Stack Allocations (REPORT FORM)~~ — RESOLVED
Fixed: `cmd_report_form` now heap-allocates `frm_def_t` (~14KB) instead of
placing it on the stack.

### 3.7 AST Constant Folding — WON'T FIX
Real dBase expressions are nearly all field/variable references and function
calls. Pure constant subexpressions are extremely rare in practice. The
significant win (compile once, evaluate many) was already captured in 2.4.

---

## 6. February 2026 Review II: Problems & Opportunities

### 6.1 ~~Broken Numeric Indexes~~ — RESOLVED
Fixed: numeric index keys are now canonicalized to a fixed-width sortable
encoding (transformed IEEE-754 hex) instead of variable-length display strings.
This restores correct numeric ordering under `memcmp` and keeps key width stable.

### 6.2 ~~Incorrect Numeric Semantics in `SORT`~~ — RESOLVED
Fixed: `SORT` now derives type-aware keys for numeric/date fields using the
same canonical key formatter as indexing. Numeric sorts are now true numeric
order across sign boundaries (e.g., `-10 < -1 < 1 < 10`) rather than raw
ASCII field-text order.

### 6.3 ~~UDF Host Stack Recursion~~ — MITIGATED
Fixed hard-failure path: recursive UDF calls now fail with a controlled
`ERR_STACK_OVERFLOW` (`*** UDF recursion overflow ...`) instead of crashing
the VM stack. Added `test_udf_overflow` regression coverage.

Additional hardening:

- moved function-call argument arrays in `expr.c` / `ast.c` from stack to heap
- restored UDF state cleanly on callback setup failure (no leaked UDF state)
- improved UDF callback error propagation so execution errors are not reported
  as `Unknown function`

Current behavior: recursion is bounded by a conservative UDF callback nesting
guard to stay within SLOW-32 stack constraints.

### 6.4 ~~String Concatenation `-` Compatibility Candidate~~ — RESOLVED
Fixed: string `-` now uses Clipper/FoxPro-leaning trim-concat semantics:
trim trailing spaces from the left operand, append the right operand, then
append the trimmed spaces to preserve total width (subject to 255-byte cap).
Implemented in both expression evaluators (`expr.c` and `ast.c`) with
regression coverage (`test_string_minus_ast`) and updated expression expected
output.

### 6.5 ~~Index Key Volatility~~ — RESOLVED
Fixed: date index keys now use canonical DBF format (`YYYYMMDD`) internally
for both index build/maintenance and `SEEK` key generation. Index behavior is
now independent of `SET DATE` display format.

### 6.6 ~~Index AST Caching~~ — RESOLVED
Implemented: each index now caches a compiled key-expression AST when safe
(no `&` macro in expression). Index build and key-maintenance paths evaluate
through `ast_eval` when cached, falling back to dynamic string evaluation for
macroized expressions. Cached AST is refreshed on index rebuild/open and freed
on close.

### 6.7 ~~Standard-Compliant Index Keys~~ — RESOLVED (Superseded)
Resolved by the 6.1/6.5 fixes:

- Numeric index keys now use a fixed-width sortable binary-safe encoding
  (transformed IEEE-754), preserving numeric order under `memcmp`.
- Date index keys now use canonical `YYYYMMDD` internally (equivalent ordering
  behavior to `DTOS()`), independent of `SET DATE`.

## 4. Test Coverage Gaps

### 4.1 Missing scenarios — MOSTLY RESOLVED
Added tests: `test_edge_fio` (FWRITE/FREAD zero/negative/invalid),
`test_edge_arrays` (boundary conditions, STORE without index),
`test_edge_varstore` (full store + DECLARE), `test_edge_calc_macro`
(CALCULATE with `&macro` expressions, nested parens).
Remaining: binary data in FREAD/FWRITE (NUL bytes truncate due to string
representation — known limitation, not testable via dBase strings).

### 4.2 ~~Fragile tests~~ — NOT AN ISSUE
`test_dir_services` already uses relative comparisons (`ADIR() > ADIR("*.PRG")`,
`ADIR("*.PRG") > 0`, `ADIR("*.ZZZ") = 0`) and committed test fixtures. No
absolute paths or brittle file counts.

---

## 5. Completed Summary (High Level)

Major completed areas (not exhaustive):

- Memo fields (.DBT) read/write, COPY/APPEND/SORT, ZAP reset
- EDIT/BROWSE full-screen editor (with no-term fallback)
- INKEY/LASTKEY/READKEY + key handlers (SET/ON KEY)
- TEXT...ENDTEXT
- SCATTER/GATHER
- CALCULATE
- Menu system: @PROMPT/MENU TO, DEFINE POPUP/BAR, DEFINE MENU/PAD, ACTIVATE,
  ON SELECTION callbacks
- AST-based expression parsing/evaluation (FOR/WHILE/FILTER/VALID/WHEN)
- Eval-time macro expansion for `&var` expressions (detect-and-recompile)
- ADIR wildcard globbing
- UNIQUE indexes and two-phase constraint checking
- Date arithmetic, path normalization, cross-area cache invalidation
- Stress test suite (strings, recursion, big DB, indexes, work areas)

### Procedure lookup rescanned the source on every UDF call — RESOLVED 2026-09-05

majesty's `run_dbase_s32.sh` took 46 s against 0.26 s for the same books in
COBOL. A native build of the interpreter (host `qsort_r` takes BSD argument
order, so a shim is needed) ran the import in 8.3 s, so the guest was only the
usual ~3.3x of native: the time was the interpreter's own algorithms.

- `find_procedure` walked every line of the current program and then the
  procedure file on every user-function call, copying and macro-expanding each
  line to see whether it began with PROCEDURE or FUNCTION. The import's index
  build evaluates three PAD_ZERO calls per record over 55k records. Now each
  `program_t` builds a name/line table on its first lookup (the text never
  changes after load). Half the native samples were here.
- REPLACE re-read the current record from disk before replacing it, then
  flushed after every field: two seeks, a read, a write and a flush per field,
  eleven fields per record in the merge join. `dbf_read_record` now returns at
  once when asked for the current record while it is dirty (the buffer holds
  exactly those bytes), and `replace_cb` leaves the record dirty unless an
  index is open (the UNIQUE rollback re-reads the disk copy) or the same file
  is open in another work area (which reads the disk; `test_stress_workarea`
  caught this one). Every movement flushes first; the six places in command.c
  that moved to EOF by assigning `current_record` now go through
  `dbf_move_eof`, which flushes -- a bare assignment left the dirty record to
  be written one slot past the end on close (every file grew by one record in
  the first prototype). That hole was latent while nothing was ever dirty at
  SKIP.

Gate: tests 102/102 before and after; majesty's 12 dBASE reports and the
FLATLINE/TXNS/LINES data files and FLATACCT index byte-identical. Runner
46.5 s -> 26.9 s; the REPLACE change is under a second of that on the guest.
What remains is structural: every statement is re-lexed on each execution and
`memvar_find` scans all 256 slots with a case-insensitive compare.

**Second round (2026-09-05, same day):** runner 26.9 s -> 21.8 s, reports and
data files byte-identical, 102/102.
- `push_frame` zeroed the whole 27KB frame (saved_vals, with_args) on every
  call; now the two counters and the two name arrays that readers test.
- A random cache miss read 256 records (64KB) to use one; the activity
  report visits 55k records in index order. A jump now reads 8, a
  sequential miss still the full block (`cache_next`).
- `memvar_find` walked all 256 ~300-byte slots with a case-insensitive
  compare (77KB per lookup; majesty's names mostly share a first letter, so a
  first-byte reject did little). Stored names are upper-case already: the
  query is upper-cased once, each slot carries an FNV hash of its name, and
  the walk stops at a high-water mark maintained on every fill and release.
- APPEND BLANK wrote the blank record and the header count at once, then the
  REPLACEs dirtied it again: one write per record became three (blank,
  header, flush). `dbf_append_blank_ex(db, eager)` keeps the record in the
  buffer and the count for close; `wa_writes_eager()` (index open, or file
  open in another area) is the one rule REPLACE and APPEND now share.
What leads the native profile now is the lexer: every statement is re-lexed
each time it executes.

**Third and fourth rounds (2026-09-05):** runner 21.8 s -> ~17 s.
- `func_call` walked the 124-entry builtin table with strcmp on every call
  (a user-defined function had to miss all of it first): hashed on first use.
- `lexer_init_ext` zeroed 2.4KB per statement and sub-clause, most of it the
  macro stack read only below `macro_depth`: now the token and a few fields.
- Index page cache 64 -> 512 pages (FLATACCT is ~600 pages; every SEEK
  missed on its leaf).
- Compiled-expression cache (`ast_cached` in ast.c, 1024 entries keyed by the
  expression text): `ast_eval_dynamic` no longer compiles, evaluates and frees
  on every call, and IF, RETURN, STORE, SEEK, `?` and assignment -- the fresh
  recursive-descent parse in expr.c on every execution -- go through it
  (`ast_eval_adv` is the advancing form). The two rules from 2.1/2.2 hold:
  text with `&` is never cached; the cache is dropped whenever an array is
  declared or freed (`memvar_array_gen`), because name(...) is an array or a
  call according to what exists at compile time. Trees are never written
  during evaluation, so one tree serves every work-area context.
Gate each round: dbase tests 102/102; majesty's 12 reports, data files and
indexes byte-identical to the previous binary's.

**Fifth round (2026-09-05):** runner 17.1 s -> 15.4 s.
- `dbf_flush_record` synced its own cache and then called
  `area_invalidate_all`, which dropped it again: every REPLACE-then-SKIP loop
  read a full block per record. `area_invalidate_others(filename, db)`.
- Seek elision: `file_pos` plus `last_op` (read/write) skip the `fseek` when
  the stream already sits at the record and the last operation was of the
  same kind (a seek is still needed to switch between reading and writing).
- `prog_run` made its comment / PROCEDURE / IF / CASE / TEXT tests on every
  line on every execution (`line_is_kw` tokenises the line each time).
  `prog_line_kind()` decides once per line (`program_t.kind`), with the same
  predicates in the same relative order; a line holding `&` is classified
  fresh each time because its text can change.

**Sixth round (2026-09-05):** runner 15.4 s -> 13.2 s.
- **The index page cache never worked for a work-area index.** `index_init()`
  sets the capacity, but the command layer never calls it: the slots in
  `areas[]` are zeroed, so `cache_capacity` was 0 and `cache_evict()` freed
  every unpinned page after each fetch -- a SEEK re-read its whole
  root-to-leaf path on every call, and a build evicted as it went. The
  earlier "64 -> 512 pages" change therefore changed nothing.
  `index_cache_setup()` applies the capacity at open (`index_read`) and at
  build. Found by the profile still showing a page read per SEEK on a
  five-page index after that change.
- `is_keyword` measured both strings on every call before comparing (the
  four-character rule); it now rejects on the first letter first, which is
  most calls. `dbf_find_field` likewise (field names are upper-case).

**Seventh round (2026-09-05):** runner 13.2 s -> 12.7 s; native report step
sys time halved.
- Record windows beside the sequential block: a non-sequential read fills the
  aligned 8-record block holding the record into a direct-mapped slot
  (`nwin` = blocks in the file, <= 2048, so <= 4MB); the activity report's
  second pass over an account now hits what the first pass read. (A first
  version with 64 LRU windows gained nothing: an account's history runs to
  thousands of lines and evicted itself.)
- `str_copy` was a byte loop and showed in every profile: length + memcpy.
- `prog_run` ran `prog_preprocess` on every line; both things it does (the
  `&&` comment and `&macro`) start with `&`, so a macro-free line is skipped.

Where it stands: 46.5 s -> 12.7 s over the day, every round gated on
102/102 and byte-identical reports, data files and indexes. What is left is
per-statement: each executed line is copied, classified, dispatched and its
clauses re-lexed (REPLACE re-parses "field WITH" for every record), and every
modified record is written through on its own (one write per record in a
REPLACE-then-SKIP loop, one per APPEND). The next steps of size would be a
parsed-statement cache per line and a write-back cache block; both are
design changes rather than fixes.


### A table had no end byte, and PACK and ZAP left deleted records in the file — RESOLVED 2026-11-15

Found running Teacher's Pet beside its own 1980s build (WordTech Quicksilver
1.2c under a DOS emulator) on the same tables: its index program PACKs every
table, and afterwards ours were the old length with the header's count
lower, the deleted records still on disk past the last live one, and no
0x1A after it.  dBASE III ends a table with 0x1A straight after the last
record and nothing past it; ours never wrote that byte -- not at CREATE,
APPEND, PACK or ZAP -- and PACK and ZAP never shortened the file.  Readers
here go by the header's count, so no answer changed; another program
reading to the end of the file, or anyone looking at it, would find the
records the user had removed.  `dbf_set_end` writes the byte and truncates
after it: at CREATE (after no records), PACK, ZAP, and at close when
records were appended.  Test: tests/test_dbf_end.txt (file lengths and the
last byte through FSEEK/FREAD).

### A 0x1A in a program file was read as program text — RESOLVED 2026-11-15

DOS editors and COPY padded sources out with 0x1A, the end-of-file byte,
and dBASE stops reading a .PRG at the first one.  We read on: Teacher's
Pet's TPSO.PRG, as recovered, ends in a run of them, and both engines
reported "Unrecognized command" on the line after its last statement (45
of the 80 recovered sources carry them).  prog_fgets (program.c) cuts a
line at a 0x1A and ends the file there; the interpreter's loader and
s32-xbc both read through it.  Test: xbc/tests/dosend.prg and dosendp.prg
(text after the padding, which must not run), in the suite and the oracle.

### A field GET wrote its value's string raw: numbers left-justified, indexes not kept — RESOLVED 2026-11-15

Teacher's Pet's System Options screen GETs two N(2) fields; accepting
them unchanged rewrote " 5" as "5 " in the table, where its Quicksilver
build left the record as it was.  get_store_value (screen.c) put
val_to_string's text into the record with dbf_set_field_raw: no field
formatting (a number left-justified, decimals unaligned) and no index
update, so a GET on a key field left its index stale.  It stores through
cmd_api_field_assign now, REPLACE's path.  Test: test_get_field_num (the
record's bytes after READ, kept and typed; the old build fails it).

### A GET on a variable named x never stored; a number read through a GET was ten wide — RESOLVED 2026-11-15

Two defects in the GET path, found by running Teacher's Pet against its
Quicksilver build.  (1) A variable's GET reads and writes through a block,
{|x| IIF(PCOUNT() == 0, <target>, <target> := x)}; a variable named x was
hidden by the parameter, and READ assigned the parameter to itself -- the
variable kept its old value.  MEMVARBLOCK and FIELDBLOCK built their
blocks the same way.  The parameter is _1 now, as Clipper's preprocessor
names it.  (2) The GET editor gave back val_num(atof(buffer)), a number
of no width, so `@ 4,41 SAY T_quar` after a one-column GET printed ten
columns where Quicksilver (and Clipper) print one.  getsys_number_from_text
(getsys.c) sizes the number by the GET's field, for the Get object's
untransform and for the editor alike.  Test: test_get_var.

### A table's last-update date was never changed after CREATE — RESOLVED 2026-11-15

dBASE III stamps the header's date (bytes 1-3, what LUPDATE() reports)
when a table is changed; Teacher's Pet's Quicksilver build does, and a
table ours had written still carried its creation date.  dbf_t.modified is
set by a record write, a count write or a new end; dbf_close stamps today
then.  A table only read keeps its date.  Test: test_dbf_lupdate.

### A numeric GET in the interpreter cleared on the first key, as Clipper's does — RESOLVED 2026-11-15

User ruling (2026-11-15): the interpreter edits as dBASE III does, a
compiled program as Clipper does.  Teacher's Pet's Quicksilver build
shows dBASE III's numeric GET: "1" typed over 0 in "##" shows "10" -- the
old digit stays on the screen past what is typed -- and the value taken
is 1, the field cleared as Clipper clears it.  (A first change took the
"10" on the screen for the value; the next screen, where the program
finds type 1, corrected it.)  So the value is cleared in both engines,
and in the interpreter the old text stays shown past what is typed until
the GET is left or edited otherwise (get_shown).  Test: test_get_overtype
(term service, keys piped, as the INKEY cases; the value is 1).

### Keys typed while a program ran were thrown away; ended input was polled forever — RESOLVED 2026-11-15

SET ESCAPE's check between statements (screen_check_escape) read any key
waiting and, unless it was Esc, discarded it ("can't push back") -- dBASE
keeps type-ahead for the next read.  And once piped input had ended, the
terminal kept reading ready and the check read end of input before every
statement.  A key other than Esc goes to the type-ahead buffer now (as
read_dbase_key translates it, F-keys included), and ended input is not
polled again.  Found running Teacher's Pet in the interpreter with piped
keys, beside its Quicksilver build.  Test: test_typeahead (the old build
loses the key and loops until its timeout).

### Character GETs: a long template widened the field, and the value came back trimmed — RESOLVED 2026-11-15

Teacher's Pet's comment screen GETs SPACE(1) with PICTURE "!A"; its
Quicksilver build leaves the GET as the letter is typed, and Harbour's Get
is one column wide too.  Ours made the field the template's width and
waited for a second key.  And the value went back cut to its last
non-blank: SPACE(3) accepted as it stood came back "" (so `c = " "` was
false and the program took the blank answer for a letter), "ab" typed for
SPACE(5) came back two long.  dBASE III and Clipper keep the GET's width.
The editor clamps a character field to the string when the template is
longer (not under @R, whose literals are not kept); the value is trimmed
only for numbers, dates and logicals; get_text_value pads a string to its
GET's width (line mode's text is the line alone).  Tests:
test_char_get, test_get_var.

### A GET with a template was drawn without its literals until it was reached — RESOLVED 2026-11-15

READ drew each GET's value as it stood; a template's literals appeared
only when the editor reached the field.  Teacher's Pet's new-student
screen, under its Quicksilver build, shows the telephone GET as
"   -    " while the name GETs above it are edited.  get_show_initial
draws a character GET with a template through TRANSFORM (which already
matched Harbour's), as get_show_value does after an edit.  Test:
test_template_get.

### Date and logical field GETs edited raw text; an untouched GET was assigned — RESOLVED 2026-11-15

A field GET on a D or L field was set up as a character GET holding the
raw record text ("19940124").  With the field GET storing through
REPLACE's path (earlier the same day) an accepted date came back blank,
and before it a typed date was lost.  D fields now edit as dates (SET
DATE, ten wide with CENTURY) and L fields as logicals.  And READ assigned
every GET it passed through: a blank "999-9999" telephone field, drawn
"   -    ", was stored as that.  dBASE's READ and Clipper's Get assign
only a GET whose buffer changed; one left as it began is not assigned.
A blank field's value is padded to its width before TRANSFORM draws it.
Tests: test_field_get_kept, test_field_get_typed (both fail on the build
of the day before).

### The printer was the screen — RESOLVED 2026-11-15 (user ruling)

With no SET PRINTER TO file, printed output (SET DEVICE TO PRINTER's
@SAY, ? under SET PRINTER ON, REPORT FORM TO PRINT) went to the console
and landed on the screen among the program's own output, and EJECT wrote
"--- PAGE ---" there.  dBASE sends it to PRN, the printer port.  User
ruling (2026-11-15): the printer is a file, in both engines -- SET
PRINTER TO's, else $XBASE_PRN, else PRN.TXT in the current directory,
made afresh when something is first printed and flushed as it is
written; nothing printed reaches the screen.  This is what lets
Teacher's Pet's reports be held byte for byte to its Quicksilver build's
(dos-monster -prn).  Tests: test_phase2 (reads PRN.TXT back),
test_prn_file ($XBASE_PRN).

The printer's lines (same day): what dBASE sends the port, a new line is
CR LF and EJECT is CR then form feed; ours wrote LF, and form feed then
CR.  printer_put sends CR LF for every new line not already after a CR,
and REPORT FORM TO PRINT is spooled through the printer stream (it wrote
the file itself, LF alone).  Teacher's Pet's student list now prints the
same 3,285 bytes as its Quicksilver build (dos-monster -prn), both
engines.  Tests: test_phase2, test_phase6a (REPORT TO PRINT).

### SCAN and COPY TO walked record numbers under an index — RESOLVED 2026-11-15

dBASE processes a scope in the controlling index's order.  COPY TO (and
its SDF and DELIMITED forms) and SCAN -- the interpreter's and the one
compiled programs use -- stepped record numbers whatever index was in
control; the commands built on process_records did not.  Teacher's Pet's
grade report copies the grades through their period+student+date index
and re-indexes the copy on period+student: under its Quicksilver build
each student's grades come out in date order, here in record order, and
the printer, sent to columns behind its head, overprinted the line.
rec_cursor_first/next (command.c) walk a scope in index order, live, from
the current record (SCAN's step goes from where its body left the
pointer, as dBASE's SKIP does); COPY TO and both SCANs walk with it.
Test: xbc/tests/scanidx.prg (SCAN, SCAN REST, SCAN NEXT, COPY TO, COPY TO
SDF under an index; the old build fails every line), suite and oracle.

### SORT took one key and dropped the rest, a FOR after them too — RESOLVED 2026-11-15

SORT TO file ON f1, f2, f3 FOR cond: the parser read f1, stopped at the
comma, found no FOR there, and sorted every record on f1 alone, saying
nothing.  dBASE III Plus sorts on a list of fields, each with /A /D /C
(/DC together).  Teacher's Pet's progress report sorts its students ON
ST_PE_NUM,ST_LAST,ST_FIRST FOR a name range that, left blank, selects
nobody -- its Quicksilver build prints nothing, and ours printed every
student.  The keys are fixed-width segments compared each with its own
direction and case; equal keys keep their order (qsort is not stable;
the record number breaks the tie); and a SORT that selects nothing makes
its table, empty, where it refused ("No records to sort") and the
program's next USE failed.  Test: xbc/tests/sortkeys.prg (old build fails
every line), suite and oracle.

### Equal keys in an index were in no particular order — RESOLVED 2026-11-15

dBASE's and Clipper's indexes keep equal keys in record order; ours did
not.  (So it was taken to be; dBASE III PLUS itself, run later, keeps them
in the order they came, which INDEX ON and appends make record order --
see "Index files were dBASE III's format but not its trees" below.)  INDEX ON sorted with qsort, which is not stable, and an insert went
ahead of the equal keys already in its leaf.  Teacher's Pet's grade
report copies a student's grades in date order and walks them through an
index on the student alone: under its Quicksilver build the dates come
out in order, here they broke off partway along the line.  The build
breaks ties on the record number, and an insert goes after the equal keys
with smaller record numbers.  (An insert into a run of equal keys that
spans leaves, of a record older than some of them, can still land at the
start of the last leaf's run -- resolved in the next entry.)  The user's ledger reports, run with and
without the change, differ only where equal-key lines were summed in a
different order: "-0.00" became "0.00" on two lines.  Test:
xbc/tests/eqkeys.prg (the old build fails it).

### A run of equal keys across index pages: SEEK found it part-way, a delete missed an entry — RESOLVED 2026-11-15

A page's separator is the first key of the page to its right, so a run of
equal keys a split divided starts in the page LEFT of it.  The search
routed right of the separator: SEEK landed in the run part-way, and
Teacher's Pet's grade report, still after the previous fix, lost a
student's first grades.  Removing an entry routed the same way and missed
one in an earlier page, leaving it in the index; the duplicate check for
UNIQUE looked at one leaf only; and an older record taking a key already
in such a run went to the end of it.  Now the search takes the first page
that can hold the key and goes on into the next leaf when it runs off
the end, a remove tries each child the run spans, the duplicate check
walks the run across leaves, and an insert picks, among the children the
run spans, the last whose first entry is not past its record number.

Its test, xbc/tests/dupspan.prg (2,000 records under three keys, SEEK to
each, every seventh record's key replaced, the order walked), is 4,600
lines, and the interpreter refused it: a program was held to 2,000
lines.  The table grows now, and running out of memory says so.  Under
stage08 the compiled test then ran off its stack -- selfhost ISSUES-119.

### An index was this interpreter's own format: dBASE's and Quicksilver's could not be opened — RESOLVED 2026-11-15 (user ruling)

`.NDX` files were NDX2, a B+ tree of 4 KB pages with leaf links, by the
ruling of 2026-10-29 (REINDEX the interchange).  Teacher's Pet's grade
report then matched its Quicksilver build but for one line, and that line
came from the disc's own GRADES index: 2,557 entries for 2,550 records,
seven records indexed twice.  Quicksilver walked it as it was; we could
only rebuild it.  Asked whether xbase should read and write dBASE III's
format, the user chose read and write.

The pages are dBASE III's now: 512 bytes, a 16-bit key count and items of
(left page, record number, key); an internal key is the last key under
its left page (the separator had been the right page's first key, and
the insert, split, borrow and bulk-build code follow the new rule);
numeric and date keys are IEEE doubles, a date its Julian day (a date had
been its YYYYMMDD, key type 2: gone); the header has the key length,
keys per page and item size, which a reader takes as given (the disc has
items of 20 and 22 bytes for 11- and 13-byte keys).  The file keeps no
leaf links or entry count: opening reads the internal pages level by
level and rebuilds both (in key order since, depth first: dBASE's own
trees are not level -- see below).  DESCENDING and FOR, which dBASE III has not,
sit in header bytes it leaves zero (byte 22, and the FOR after the key
expression's NUL).  NDX2 and NDX1 files are converted when opened.

Held to Quicksilver on Teacher's Pet: its build prints the grade report
from the files xbase writes, both of ours print Quicksilver's from the
disc's damaged ones (26,943 bytes, the repeated grades included), and
after each of 27 edit and report sessions on the disc's files the three
leave indexes that walk the same.  An emptied page there has the count
0x80000000: the count is read, as Quicksilver reads it, by its low 16
bits.  Tests: xbc/tests/ndxkeep.prg (kept up, reopened, against fresh
builds), xbase test_ndx2_convert (tests/ndx2, made by the build before).

### Index files were dBASE III's format but not its trees — RESOLVED 2026-11-15

The user found an unprotected dBASE III PLUS 1.1 (the copies on the
discs are copy-protected, and were left so), and it runs under an
emulated PC: an oracle for the index files themselves.  Its INDEX ON
over 600 records wrote what ours did, byte for byte but for bytes it
never sets.  Kept up through appends and changes, its trees and ours
parted at once, and its own files showed a fault in ours:

- **A dBASE index of about a thousand entries walked out of order.**
  dBASE's INDEX ON fills each page and numbers it as it is finished --
  a leaf, then any internal page that leaf filled -- and at the end
  makes no page for a last child alone at its level: the child goes up
  a level.  At 993 entries of a 6-byte key the root is over an internal
  page of 32 leaves and over one leaf.  Opening rebuilt the leaf chain
  level by level, taking every leaf to be at one depth: 78 entries out
  of order, the last record wrong.  It walks the tree in key order now,
  depth first, and our INDEX ON lays pages out as dBASE's does.
- **A blank date sorted first; dBASE's key for it is 1.0E+100**, after
  every date.
- **Equal keys stay in the order they came** (the entry above took them
  to be in record order): a record REPLACEd into a key goes after those
  there, whatever their numbers.  An insert goes past every key equal to
  it, down the tree and in its leaf.
- A leaf splits with the larger half on the left, an internal page with
  it on the right (32 keys: 15, one up, 16), and an internal page holds
  as many keys as a leaf (one more than ours), its last child's pointer
  after them.
- A removal borrows from no page and merges none; a page left empty goes
  out of its parent, and its number is never used again (the file keeps
  no free list, and we kept one in memory).  An internal key is kept the
  last key under its child: when that entry goes, the key becomes the
  new last, up the tree through rightmost children.
- APPEND BLANK puts a blank key in each index before the REPLACE, which
  is why dBASE's first leaf seemed to split by a rule of its own.
- The key expression is written with a space after it, then the NUL.
- On an empty table an expression's key length is that of a blank
  record (C + DTOC(D): C's width and 8); ours guessed 10 for anything
  but fields joined by +.

Held to dBASE III PLUS: four indexes (character, numeric, date, a
compound) under 72 random programs -- tables of 0 to 700 records, keys
of 2 to 30 bytes (12 to 42 to a page), up to 600 appends and key changes,
few distinct keys or many, blank dates -- every index byte for byte the
same but for what dBASE leaves as it finds it: the header past the
expression, the high half of a page's count, a key's padding to its
item, an internal page's last item past its pointer, a page past its
items, and pages out of the tree.  Test: xbase test_dbase3_ndx
(tests/dbase3: dBASE's own indexes, those bytes made 0, compared with
ours; its INDEX ON tree, read; the build before fails all five and walks
dBASE's file out of order).  xbc/tests/dupspan.prg checked record order
among equal keys after REPLACEs; it checks key order now.

### An error in a program was reported and passed over; dBASE III asks Cancel, Ignore, or Suspend — RESOLVED 2026-11-15 (user ruling)

The interpreter printed `*** message in FILE line N` and went on with
the next statement, taken to be dBASE III's way (ruling of 2026-10-30:
compiled programs are Clipper's, the interpreter dBASE's).  dBASE III
PLUS itself, run under an emulated PC, shows the message, a `?` under
the line, the line, `Called from - C:NAME.prg` for each program it was
called from (the innermost first, procedures by their file), and asks
`Cancel, Ignore, or Suspend? (C, I, or S)`: one key, either case, the
word shown after the question; Enter is Cancel; another key is no
answer.  Cancel says `Do cancelled` and releases the program's
variables, Ignore goes on with the next statement, Suspend says `Do
suspended` and RESUME goes on after the failing one.  The CANCEL and
SUSPEND commands say the same words (they said `*** Cancelled` and
`Suspended.`).  Told this, the user ruled the split stands: the
interpreter asks as dBASE does, compiled programs stop as Clipper's.

Now so.  Where the `?` goes is dBASE's parser's own; ours stands two
past an unknown name the line has (y = nosuch + 1: under the +, as
dBASE puts it), else in the second column.  Our messages keep their own
words ("Variable not found: nosuch", where dBASE says "Variable not
found.").  At the dot prompt dBASE asks "Do you want some help? (Y/N)",
which opens its help text; that we have not, and an error there is
still reported with `***`.  A Cancel in a function the prompt's command
called is carried out when the command ends.

Two faults the question found.  SET ESCAPE's look for Esc before each
program line keeps any other key typed ahead, and a line read at the
prompt (or by ACCEPT, INPUT, CREATE) took stdin alone: a program's run
cost the next command its first characters.  read_line takes what was
typed ahead first now.  And a compiled program that had filled the
variable table could not run its default error handler (the handler's
parameter wants a slot): it printed the interpreter's `***` and went
on.  Four slots past the limit open while a handler runs, and it
reports and quits as Clipper's does.  Tests: test_error_ask (each
answer, nested calls, typed-ahead text whole; the build before fails
it); test_edge_varstore, test_index_keyerr, test_stress_index and
test_udf_overflow answer Ignore; the xbc oracle lines up the new report
with a compiled program's (73 agree).

### A variable a procedure made outlived it — RESOLVED 2026-11-15

dBASE III and Clipper release, when a program or procedure returns, the
variables it made without declaring them: x = 1 in a procedure makes a
private variable of it.  Both engines kept every such variable (only a
PRIVATE declaration was undone on return), so a program's work was
still there at the dot prompt and a procedure's scratch variables in its
caller.  dBASE III PLUS, run beside it: `w = 3` in a procedure, then
TYPE("w") in its caller, is "U"; a program's x at the dot prompt is
"Variable not found."; a cancelled program's variables are gone.

A variable now records when it was made (a serial) and whether PUBLIC
made it.  A frame's return releases what was made at its depth, PUBLICs
and LOCALs aside, before the PRIVATEs' earlier values come back (at the
caller's depth: they had been made again at the callee's); a run from
the prompt releases at its end what it made at the top level, unless it
is suspended; CANCEL does both for every level.  The compiled program's
return (clip_leave) does the same.  A variable the callee finds already
made is the caller's and keeps what it is given, as before.  Tests:
xbc/tests/mvscope.prg (both engines; the oracle holds them together);
test_do_prg, test_procedure and test_phase5 read a program's variable at
the prompt after it ended, and now see none.  The user's ledger reports
and Teacher's Pet are unchanged.

### The lab's Quicksilver programs: macros in commands, SUM lists, a zero divisor, ten-character names — RESOLVED 2026-11-15

Five programs from the hospital laboratory's discs, Quicksilver-built
(the correlation study CORRPT, donor labels GR/ADDLAB, the analyser
capture loader LDAS, and ND), run beside their own executables under an
emulated PC, and beside dBASE III PLUS where the two part.  Synthetic
data stood in for the donor table and the capture file; the correlation
tables hold specimen QC, compared by checksum.  CORRPT now prints what
its Quicksilver build prints, byte for byte, in both engines, and leaves
its tables the same; GR's labels match; LDAS reads its file as dBASE
does.  What it took:

- **A command with a macro lost the rest of its line, and could
  overwrite memory.**  A token read out of a macro's expansion has its
  token_start in the expansion, so the text a command measured from it
  -- REPLACE &fld WITH e (the field, then nothing: "Expected expression
  after WITH", or a silent 0), FOR &c and WHILE &c (TO n lost), SUM,
  AVERAGE and STORE of &e -- was cut off at the expansion's end.  INDEX
  ON &k TO f measured from the expansion to the line's TO: a negative
  length, and memcpy wrote past the key (the DBT faulted; memcheck named
  the read).  They measure in the line itself now (lex_src: the & of
  the macro); INDEX ON writes the expanded key in the header, as dBASE.
- **SUM and AVERAGE took one expression.**  dBASE III's take a list
  (none: every numeric field) TO a list; `SUM a, b TO x, y FOR c` summed
  a, ignored b and y and the FOR.  A total keeps its expression's
  decimals; an average does too in the interpreter, as dBASE's (an N(5)
  field averaging 25.5 shows 26), and has SET DECIMALS compiled, as
  Clipper's.
- **A zero divisor stopped the interpreter.**  dBASE III gives no error:
  an overflow, shown as asterisks, false to every comparison (= and <>
  too), still one after more arithmetic; a field given it is filled
  with asterisks.  The interpreter does that now (a NaN); a program's
  error handler still sees the error, and a compiled program raises it
  as Clipper's.  MOD(x, 0) is x (it was an error).
- **A name past ten characters was not its field's.**  COR_PER's field
  is P_URIC_ACI; the program says p_URIC_ACID.  dBASE and Clipper keep
  ten characters; field lookup kept them all.
- **CLOSE ALL released the variables** (CORRPT's date was gone for its
  second report).  It closes files; CLEAR ALL releases.
- **APPEND FROM ... DELIMITED read a CSV**: a doubled quote kept, leading
  blanks kept, a last line without a line end read.  dBASE III PLUS
  ends a quoted field at its next quote, drops the blanks, and does not
  read that line; the interpreter reads it so now (Quicksilver itself
  differs from dBASE on two of the three).  A compiled program reads as
  before.

Left as they are: PACK shortens the file, where dBASE leaves the old
records' bytes past the end; the widths dBASE gives computed numbers
(24/4 shows "  6.00", a product wider) are not ours; TALK's SUM and
AVERAGE layout, and NDX() without the drive.  Tests: xbc/tests/macrocmd.prg
(both engines), test_div_overflow and test_append_dbase3 (the build
before fails both); ONERR2 and test_phase5 catch a type mismatch where
they caught a zero divisor.

### The interpreter showed numbers, listings and TALK as Clipper does — RESOLVED 2026-11-15

dBASE III gives every number a width and shows it at that width; the
interpreter used Clipper's automatic ten columns for nearly everything.
Measured against dBASE III PLUS (268 expressions, every pair of seven
field shapes under + * /), the rules, integer width n and decimals d:

- a literal as wide as it is written (5 is one, 1. two); a field its own;
- + and -: the wider n and one, the more d; *: n1 + n2 + 1, d1 + d2;
  /: max(n1 + d2, n2 + d1) + 1, d the most of SET DECIMALS, d1 and d2;
  a sign one column more;
- an assignment's variable ten wide when its value fits in ten, else the
  expression's whole width (x = x + 1 stays ten); a GET's variable its
  picture's width; a function's result as a variable would hold it;
- INT() the argument's whole width and no decimals; ROUND() one wider,
  the argument's decimals, a half up (ROUND(-2.5, 0) is -2) and a
  negative count to tens; MOD() as +, its remainder the divisor's sign
  (MOD(-123, 7) is 3: it was -4, in both dialects); MAX/MIN the wider and
  the more decimals; SQRT/LOG the argument's width, VAL the string's,
  both SET DECIMALS; STR(n) ten and no decimals; DOW() two;
- a number shown as zero without its sign (-1/300 is 0.00).

LIST and DISPLAY are dBASE's: a heading (SET HEADING is on by default in
the interpreter) of the expressions as typed, Record# unless OFF, each
column as wide as its value or its heading, numbers right, the last
column unpadded, a blank line after; DISPLAY with no list is the same
row, not a column of fields, and past the end shows the heading alone.
An expression list was taken as fields (LIST A * F listed A), a leading
OFF dropped the list, and a logical showed as T.  LIST STRUCTURE is
dBASE's layout, its last update today once the table has been written.
TRANSFORM(n, "") is ten wide with SET DECIMALS.

TALK speaks dBASE's lines, through the console stream (they never
reached the alternate file): "      1 record replaced", "Record No.",
"Record =", the count of COUNT, SUM's and AVERAGE's table, the value of
an assignment, "No find.", INDEX's and SORT's percentages, "No records
deleted" -- and nothing for USE, APPEND BLANK, SET FILTER and the rest
dBASE says nothing for.  DELETE and RECALL past the end are no error.
SORT takes ON before TO.  A compiled program keeps Clipper's display;
XBASE_WIDTHS=clipper gives the interpreter it too, which the xbc oracle
uses (the xbc compiler declares Clipper's rules itself: its parser had
sized literals by dBASE's).  The ledger reports compiled are unchanged;
interpreted, three totals of -0.00 show 0.00.  Test: test_dbase3_display
(tests/dbase3/WIDTHS.TXT is dBASE III PLUS's own output for WIDTHS.PRG;
the build before fails it); 94 expected outputs follow the new display.

Not done: the drive (C:) dBASE puts before file names.

### SET SAFETY asked nothing — RESOLVED 2026-11-15

dBASE III starts with SAFETY ON and asks before a command writes over a
file -- CREATE, COPY TO (and STRUCTURE, DELIMITED, SDF), SORT, TOTAL,
INDEX ON, SAVE TO, COPY FILE: "TC.ndx already exists, overwrite it?
(Y/N)", the name as the command completed it (COPY FILE's as typed) --
and before ZAP ("Zap T.dbf? (Y/N)").  The interpreter asks now, word for
word as dBASE III PLUS does (each command measured, answered both ways).
Yes goes on.  No to ZAP leaves the table; No to an overwrite, in a
program, is an error the question itself reports: the "Called from"
lines and Cancel, Ignore, or Suspend follow it directly.  No input (a
script that has run out) answers No.  SET ALTERNATE TO does not ask, in
dBASE either.  A compiled program and XBASE_WIDTHS=clipper never ask
(Clipper has no such questions); DBCREATE() never asks.  The ledger's
programs say SET SAFETY OFF beside SET TALK OFF now; their interpreted
reports are byte-identical to the build before.  Test: test_safety; four
tests that ZAP or write twice say SET SAFETY OFF.

Seen, not done: dBASE refuses to SORT a table of one record ("Not
enough records to sort."); xbase sorts it.

### SET ORDER TO took only a literal — RESOLVED 2026-11-15

`SET ORDER TO n` with n a variable, or any expression, left the order as
it was, silently, in both engines (the compiled program hands SET ORDER
to the interpreter's code).  It takes a numeric expression now, and TO
alone is natural order, as in dBASE III.  Found when a test walking
each order by `SET ORDER TO o` passed by walking the first four times.

### A record added or changed went into a FOR index whatever its condition said — RESOLVED 2026-11-15

INDEX ON ... FOR built the index right, but APPEND, REPLACE, GATHER and
BROWSE kept it up as if it had no condition: a blank record appended went
in, and a record changed out of the condition stayed.  A record's entry
is now its key only while the condition holds (compiled the first time,
for an index opened from its file): changing into it inserts, out of it
removes.

