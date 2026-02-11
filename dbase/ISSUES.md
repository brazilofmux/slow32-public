# dBase III Clone: Open Issues & Future Work

## 1. Known Bugs

### ~~1.1 FWRITE negative count~~ FIXED
`func.c` — Negative `count` now clamped to zero.

### ~~1.2 Array dimension overflow~~ FIXED
`memvar.c` — Dimensions capped at 8192 total elements; oversized rejected.

### ~~1.3 VALID rejection leaves dirty memvar~~ FIXED
`screen.c` — Old value saved before set, restored on VALID failure.

### ~~1.4 ADIR pattern filter~~ FIXED
`util.c` — `str_like()` rewritten as a proper glob matcher supporting `*`/`?`.

### ~~1.5 FREAD truncation~~ FIXED
`func.c` — Read count capped at 255 (sizeof `value_t.str` - 1).

### ~~1.6 Memory leak on full variable store~~ FIXED
`memvar.c` — `memvar_declare_array` frees allocations if `memvar_set` fails.

### ~~1.7 FOPEN/FCREATE ll_error~~ FIXED
`func.c` — Both now set `ll_error = -1` when handle table full.

### ~~1.8 Duplicate FILE entry~~ FIXED
`func.c` — Duplicate removed; single entry remains.

### ~~1.9 VALID/WHEN parse-time side effects~~ FIXED
`screen.c` — `ast_compile_adv()` now parses without evaluating, advancing the
pointer with zero side effects. VALID/WHEN ASTs compiled in `screen_get`,
evaluated during `screen_read`.

### ~~1.10 WHEN/VALID eval errors~~ FIXED
`screen.c` — AST eval checks both return value and `VAL_LOGIC` type; validation
rejects on eval failure or non-logical result.

### ~~1.11 Term-mode READ ignores VALID and WHEN~~ FIXED
`screen.c` — Term-mode READ now checks WHEN (skip field) and VALID (save/restore
on failure) matching line-mode behavior.

### ~~1.12 Bracket string syntax~~ FIXED
`lex.c` — Context-sensitive lexing: `[` after identifier/`)`/`]` = array index;
`[` at expression start = string literal.

### ~~1.13 ensure_dbf_ext path case~~ FIXED
`command.c` — `upper_basename()` uppercases only the filename portion, not
directory components.

---

## 2. Compatibility Gaps

These are features a dBase III/Clipper/FoxPro programmer would expect.

### ~~2.1 Memo Fields (.DBT)~~ FIXED
Full read-write memo support implemented. 512-byte block `.DBT` files. CREATE
with type 'M', REPLACE writes memo text, LIST shows "memo" marker, DISPLAY and
expressions read full text. COPY TO / APPEND FROM / SORT copy memo data to new
`.DBT` with fresh block numbers. ZAP resets `.DBT` header. Memo text truncated
to 255 chars on read (`value_t.str[256]`). Grow-only writes (matches real dBase
III). Missing `.DBT` on open warns and returns empty strings.

### 2.2 EDIT/BROWSE — Full-Screen Record Editor
`EDIT` and `BROWSE` are the primary interactive data entry commands in dBase.
`BROWSE` displays a spreadsheet-style grid of records with scrolling,
field-by-field editing, and insert/delete. This is a large feature (several
hundred lines), but it's the command most dBase users reach for first.

- Requires terminal mode (already available via `term.h`)
- Core operations: scroll up/down, edit field in place, append, delete
- Clipper extended BROWSE with code blocks for custom behavior

### ~~2.3 INKEY()/LASTKEY()/READKEY() — Keyboard Input~~ FIXED
Fully implemented with terminal support. `INKEY()` polls, `INKEY(0)` waits
forever, `INKEY(n)` waits n seconds. Arrow keys and special keys translated
from ANSI escape sequences to dBase key codes (Up=5, Down=24, Left=19,
Right=4, PgUp=18, PgDn=3, Home=1, End=6, Ins=22, Del=7). `LASTKEY()` tracks
the last key from INKEY/READ/WAIT. `READKEY()` returns 12 (normal exit) or
36 (Escape). READ Escape now exits the entire READ (skips remaining GETs).
Falls back to `getchar()` when terminal service is unavailable.

### ~~2.4 TEXT...ENDTEXT — Block Text Output~~ FIXED
Implemented in `program.c:prog_execute_line()`. TEXT outputs every subsequent
line verbatim (no preprocessing, no macro expansion) until ENDTEXT. Correctly
skipped inside false IF/ELSE branches and non-matching CASE blocks. Program-only
construct (not available in interactive mode).

### ~~2.5 SCATTER/GATHER — Record-to-Memvar Transfer~~ FIXED
`SCATTER` copies current record's fields into memory variables (one per field,
named after the field). `GATHER` writes them back with proper index maintenance.
Both support `FIELDS field1,field2,...` to limit which fields are transferred,
and accept optional `MEMVAR`/`FROM MEMVAR` keywords for dBase III compatibility.
All field types handled: C, N, D, L, M.

### ~~2.6 SET Commands — Missing Options~~ FIXED
All commonly-used SET options are now implemented:

| SET | Status |
|-----|--------|
| SET TALK ON/OFF | Done (original) |
| SET HEADING ON/OFF | Done (original) |
| SET SAFETY ON/OFF | Done (original) |
| SET BELL ON/OFF | Done (original) |
| SET DATE | Done (original) |
| SET DECIMALS | Done (original) |
| SET ECHO ON/OFF | Done — echoes `[file:line] cmd` during DO |
| SET CENTURY ON/OFF | Done — 2/4-digit year in all date formats |
| SET ESCAPE ON/OFF | Done — Esc interrupts programs (requires term) |
| SET MARGIN TO n | Done — stored (0-254), used by printer output |

### ~~2.7 ON KEY — Event-Driven Key Handling~~ FIXED
`SET KEY <keycode> TO <procedure>` registers a handler procedure called when
that key is pressed during WAIT/READ/INKEY. `SET KEY <n> TO` (no procedure)
clears a single handler. `ON KEY` clears all handlers. Re-entrancy guard
prevents recursive handler invocation. Up to 32 simultaneous key handlers.

### ~~2.8 CALCULATE — Aggregate Expressions~~ FIXED
`CALCULATE` computes multiple aggregate values in a single pass, storing results
in memory variables. Supports SUM(), AVG(), MIN(), MAX(), CNT()/COUNT() with
arbitrary expressions inside parentheses. Accepts FOR/WHILE/scope clauses after
the TO variable list.
```
CALCULATE AVG(salary), MAX(salary), MIN(salary), CNT() TO avg_s, max_s, min_s, cnt
CALCULATE SUM(qty*price) TO total FOR category="A"
```

### 2.9 DEFINE/ACTIVATE MENU — Menu System
Clipper's menu system (`DEFINE MENU`, `DEFINE PAD`, `ACTIVATE MENU`) and popup
menus (`DEFINE POPUP`, `DEFINE BAR`) are the standard UI framework for
non-trivial dBase applications. Without them, menu-driven apps require manual
`@SAY`/`INKEY()` loops.

---

## 3. Robustness & Stress Testing

### 3.1 Stack Depth / Recursion Limits
The call stack is 32 frames deep (`MAX_CALL_DEPTH=32`). This is reasonable for
typical dBase programs, but recursive algorithms (tree traversal, quicksort,
recursive descent parsers written in dBase) could hit it. Questions to answer:

- What happens when recursion exceeds 32 frames? Clean error or crash?
- Does the SLOW-32 hardware stack survive 32 frames of UDF calls with local
  variables? (Each frame pushes PRIVATE vars, parameters, return address.)
- Can we increase to 64 or 128 without blowing the SLOW-32 stack?
- Write a stress test: recursive factorial, recursive Fibonacci, recursive
  tree walk. Measure actual stack consumption per frame.

### 3.2 Large Database Stress
- INSERT 10,000+ records and verify B+ tree integrity
- PACK a 10,000-record database with 50% deletions
- SORT a 10,000-record database on multiple keys
- INDEX ON with 10,000 records — verify leaf chain, page count, seek correctness
- Multiple indexes maintained during bulk APPEND FROM

### 3.3 String Boundary Conditions
- `value_t.str[256]` is the hard limit. What happens with:
  - `REPLICATE("X", 300)` — truncation or overflow?
  - `SUBSTR(long_string, 1, 300)` — truncation or overflow?
  - Concatenation that exceeds 255 characters?
  - Field values > 254 characters (C field max is 254)?
- Audit all string functions for consistent truncation behavior.

### 3.4 Index Integrity Under Mutation
- REPLACE in a loop that changes key order: does the active index stay valid?
- DELETE + PACK with multiple active UNIQUE indexes
- APPEND FROM into a database with 3 active UNIQUE indexes where source has
  duplicates — verify graceful rejection per-record
- ZAP + immediate re-insert cycle

### 3.5 Multi-Work-Area Interactions
- Same database open in two work areas, modify in one, read in the other
- SET RELATION chain across 3 work areas — verify navigation
- PACK in one work area while another has the same file open

---

## 4. Architecture Exploration

### ~~4.1 Abstract Syntax Tree (AST)~~ IMPLEMENTED
Expressions are now compiled to an AST once and evaluated by tree-walk on each
iteration. `ast.h`/`ast.c` (~580 lines) provide `ast_compile`, `ast_eval`, and
`ast_free`. Integrated into:

- **`process_records`** — FOR/WHILE clause ASTs compiled at entry, evaluated
  per-record, freed at exit.
- **DO WHILE** — Condition AST compiled in `prog_do`, evaluated in `prog_enddo`.
- **SCAN FOR/WHILE** — Clause ASTs compiled in `prog_scan`, evaluated in
  `scan_record_matches`.
- **SET FILTER** — Filter AST stored in work area, evaluated in `check_filter`.
- **VALID/WHEN** — ASTs compiled in `screen_get`, evaluated in `screen_read`.
  Fixes bug 1.9 (no parse-time side effects).

String-based `expr_eval_str` retained as fallback for graceful degradation.

### 4.2 Constant Folding
Evaluate constant subexpressions at parse time: `2 + 3` becomes `5`,
`"HELLO" + " WORLD"` becomes `"HELLO WORLD"`. Only meaningful with an AST
(section 4.1). In a dBase interpreter, the opportunities are limited because
most expressions reference fields or variables, which are inherently
non-constant.

**Where it helps**: Index key expressions like `UPPER(LNAME) + UPPER(FNAME)`
— the `UPPER()` calls can't be folded, but if someone writes
`STR(YEAR(DATE()), 4)` in a key expression, `DATE()` is constant for the
duration of an INDEX ON. Very niche.

**Verdict**: Not worth implementing standalone. If an AST is built (4.1), add
constant folding as a 20-line optimization pass over the tree. Otherwise, skip.

### 4.3 Bytecode Compilation
The natural extension beyond AST: compile the AST to a simple stack-based
bytecode. Would eliminate tree-walk overhead for hot loops. dBase programs are
typically I/O-bound (disk reads dominate), so the interpreter overhead is rarely
the bottleneck. This would be an interesting systems programming exercise but
unlikely to produce measurable speedup for real workloads.

**Verdict**: Fun project. Low practical value. Only consider after AST is stable.

---

## 5. Polish & Completeness

### ~~5.1 ADIR() with DOS-style Wildcards~~ FIXED
Resolved by 1.4 fix. `str_like()` now supports `*`/`?` glob patterns natively.

### 5.2 Error Handling Completeness
- `ON ERROR` handler exists but some error paths bypass it (e.g., UNIQUE
  violation prints directly to stdout instead of routing through `prog_error`)
- No `ERROR()` code for UNIQUE violations, file handle exhaustion, etc.
- No `RETRY` support for recoverable errors in data entry

### ~~5.3 DISPLAY MEMORY~~ FIXED
`DISPLAY MEMORY` shows all active memory variables, their types, values, and
scope (PUBLIC/PRIVATE). Implemented via `memvar_display()`.

### ~~5.4 DISPLAY STATUS~~ FIXED
`DISPLAY STATUS` shows open databases (with aliases), active indexes (master
and secondary), filter expressions, relations, procedure file, and all SET
options. Iterates all 10 work areas.

### 5.5 LIST STRUCTURE TO filename
`LIST STRUCTURE` (or `COPY STRUCTURE EXTENDED`) creates a DBF containing the
field definitions of the current database. Used for dynamic schema operations.

### 5.6 SAVE SCREEN / RESTORE SCREEN
Save and restore the terminal contents. Used for popup dialogs and help screens
that overlay the main display and then restore it.

---

## 6. Test Coverage Gaps

### 6.1 Tests That Mask Bugs
- ~~`test_dir_services.expected` — ADIR pattern count matches broken output (1.4)~~ FIXED
- `test_get_validation` — only tests line-mode; term-mode untested (1.11 fixed but not testable in automated harness)

### 6.2 Missing Test Scenarios
- Negative/zero arguments to FWRITE, FREAD, PADR, PADL, PADC
- Array boundary conditions: `arr[0]`, `arr[rows+1]`, `arr[-1]`
- Recursion depth: what happens at frame 33?
- REPLACE on non-UNIQUE index with active UNIQUE index (multi-index interaction)
- STORE to array without index (should error per 585f594, needs test)
- Binary data in FREAD/FWRITE (NUL bytes)
- String overflow: concatenation beyond 255 chars
- Full variable store (256 vars), then DECLARE array

### 6.3 Fragile Tests
- `test_dir_services` embeds absolute path and file count — breaks if repo moves
  or files are added/removed. Should be restructured to test relative behavior.

---

## 8. Teacher's Pet Compatibility (akron/src_dbase3/)

Real-world 1986 dBase III+ gradebook application — 18 .prg files, 9 databases,
multi-index navigation, procedure library, @SAY/@GET screens with PICTURE/RANGE.

### Already working
SET TALK/BELL/SAFETY/CONSOLE/DEVICE, SET ECHO/MENU/STATUS/SCOREBOARD/HELP/
FUNCTION (stubbed), EXIT, LOOP, CLEAR ALL, $ operator, RELEASE ALL LIKE, PUBLIC,
CLOSE DATABASES, PICTURE (!,9,#,A,X) + RANGE, DOW(), CTOD(), DTOC(),
REPORT FORM TO PRINT, DO WITH parameters, SELECT 1-9, USE...INDEX...ALIAS,
SEEK, LOCATE/CONTINUE, DELETE FOR, COPY TO FOR, SORT ON FOR, PACK, ZAP, ERASE,
COPY FILE, multiple indexes per database.

### ~~8.1 SET ORDER TO n~~ ALREADY IMPLEMENTED
Was already in `command.c` — `h_set()` handles ORDER TO, sets `cur_wa()->order`,
and `controlling_index()` returns the active index. All navigation commands
(SKIP, GO TOP, SEEK, LIST, etc.) use it automatically.

### ~~8.2 SET PROCEDURE TO file~~ FIXED
Was already implemented but had a bug: interactive `DO procname` from procedure
file pushed a frame and returned without calling `prog_run()`, so the procedure
body never executed. Also, `pop_frame()` freed the shared procedure library on
return. Fixed both: interactive path now enters `prog_run()` at the procedure
start line, and all three free-on-return paths guard against freeing the
procedure library.

### ~~8.3 SET COLOR TO — highlight and underline~~ FIXED
`screen_set_color()` now parses `U` (underline, ANSI attr 4), `I` (inverse,
ANSI attr 7), `+` (bold, ANSI attr 1), and `*` (blink, treated as bold).
Attributes applied via `term_set_attr()` before `term_set_color()`.

### ~~8.4 READ auto-completion on filled field~~ FIXED
Term-mode READ now auto-advances to the next GET when a field fills to its
PICTURE width and SET CONFIRM is OFF (the default). `cmd_get_confirm()`
accessor added to expose the SET CONFIRM setting.

### ~~8.5 RANGE validation loop in TPSO.PRG~~ FIXED
Root cause: `@GET` with database field names (e.g. `SY_CONDUCT` from TPSY.DBF)
fell through to empty-string default because `memvar_find()` failed. Now
`screen_get()` tries `dbf_find_field()` when memvar lookup fails, initializing
the GET with the field's current value, correct type (VAL_NUM for 'N' fields),
and field width. `screen_read()` writes accepted values back to the database
via `dbf_set_field_raw()` + `dbf_flush_record()`. RANGE/VALID rejection
messages now display on row 24 in term-mode with keypress acknowledgment.

### ~~8.6 RUN command~~ FIXED
`RUN <program>` now redirects to `DO <program>`, executing the `.PRG` source
file instead of shelling out. True exec is impossible on SLOW-32, but since
Teacher's Pet only uses `RUN TPBP` and `RUN TPPACK` to invoke other dBase
programs, the redirect produces identical behavior.

---

## 7. Review Notes (9136b895..HEAD)

These are preserved code review observations from the AST + lexer integration
work. They are not necessarily bugs, but highlight behavior shifts that should
be confirmed against dBase III expectations.

### 7.1 Macro Expansion Timing Regression
`ast_compile()` uses `lexer_init_ext()` and the current memvar store, which
expands `&var` during compilation. Previously, `expr_eval_str()` expanded
macros on each evaluation. This means `SET FILTER TO &expr` or `FOR &cond`
may no longer reflect updated macro variables after the AST is compiled.

### 7.2 Array Access Resolution Time
AST parsing treats `name(...)` as array access only if the memvar store already
contains an array at compile time. In the prior evaluator, array vs function
resolution happened at eval time. This can change behavior if arrays are
declared after a filter/loop expression is compiled.

### 7.3 Unbounded AST Function Argument Parsing
`ast_parse_primary` grows the args list without a compile-time `MAX_FUNC_ARGS`
check. Errors are deferred to evaluation, and very long argument lists can
consume memory before failing.

### 7.4 realloc Failure Handling
The argument list growth path in `ast_parse_primary` does not handle a
`realloc` failure; a `NULL` return would be dereferenced, causing a crash.
