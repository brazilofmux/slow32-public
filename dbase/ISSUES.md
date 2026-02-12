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

### 1.14 Memo leak on failed UNIQUE/index update
`command.c` — `REPLACE` and `GATHER` write new memo blocks before uniqueness
checks. If `indexes_update_current()` fails, the record is reverted but the
new memo blocks remain appended in the `.DBT`, causing file growth over time.

### 1.15 CALCULATE expression capture breaks with macro expansion
`command.c` — `cmd_calculate()` captures expression text via `token_start` while
the lexer advances. With macro expansion, `token_start` can point into the macro
stack buffer and be invalidated, leading to corrupted or truncated expressions.

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

### ~~2.2 EDIT/BROWSE — Full-Screen Record Editor~~ FIXED
`EDIT` displays one record at a time vertically; `BROWSE` shows a spreadsheet-
style grid (fields as columns, records as rows). Both support in-place field
editing with cursor navigation, delete/recall toggle (Ctrl-U), index-aware
navigation, and SET DELETED/FILTER awareness. `BROWSE` supports FIELDS clause,
horizontal scrolling, PgUp/PgDn paging, and Ctrl-N append. `CHANGE` is an
alias for `EDIT`. Falls back gracefully ("requires terminal mode") when the
terminal service is unavailable.

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

### ~~2.9 Menu System~~ DONE

The Clipper/dBase IV menu system, broken into four pieces:

#### ~~2.9a `@...PROMPT` + `MENU TO` (classic dBase III+)~~ DONE
The original lightbar menu. Define prompts at screen positions, then `MENU TO`
runs selection and stores the result (1-based index, 0 for Escape).
```
@ 10,20 PROMPT "File"     MESSAGE "File operations"
@ 11,20 PROMPT "Edit"     MESSAGE "Edit records"
@ 12,20 PROMPT "Quit"     MESSAGE "Exit program"
MENU TO choice
```
Follows the existing `@GET`/`READ` pattern: a pending prompt list, then a
command that activates the UI. Needs `SET MESSAGE TO <row>` and `SET WRAP ON/OFF`.

#### ~~2.9b `DEFINE POPUP` / `DEFINE BAR` / `ACTIVATE POPUP`~~ DONE
Standalone popup menus — a bordered box with a list of choices:
```
DEFINE POPUP fileMenu FROM 5,10 TO 15,30
DEFINE BAR 1 OF fileMenu PROMPT "Open"
DEFINE BAR 2 OF fileMenu PROMPT "--------"   && separator
DEFINE BAR 3 OF fileMenu PROMPT "Quit"
ACTIVATE POPUP fileMenu
? BAR(), PROMPT()
DEACTIVATE POPUP / RELEASE POPUP
```
Needs a popup name registry, bar list, bordered rendering, separator skipping.
`BAR()` and `PROMPT()` functions return last selection.

#### ~~2.9c `DEFINE MENU` / `DEFINE PAD` / `ACTIVATE MENU`~~ DONE
Horizontal menu bar with attached dropdown popups:
```
DEFINE MENU mainBar
DEFINE PAD pFile OF mainBar PROMPT "File" AT 0,0
DEFINE PAD pEdit OF mainBar PROMPT "Edit" AT 0,10
ON PAD pFile OF mainBar ACTIVATE POPUP fileMenu
ON PAD pEdit OF mainBar ACTIVATE POPUP editMenu
ACTIVATE MENU mainBar
RELEASE MENU mainBar
```
Two-level navigation: horizontal lightbar on pads, Enter/Down opens attached
popup, Left/Right switches pads. Most complex piece.

#### ~~2.9d Ancillary menu commands~~ DONE
- ~~`ON SELECTION POPUP/PAD ... DO <proc>` — callback on selection~~ DONE
- ~~`BAR()`, `PROMPT()`, `PAD()`, `POPUP()` — functions returning last selection~~ DONE (in 2.9b/2.9c)
- ~~`RELEASE MENU/POPUP` — cleanup~~ DONE (in 2.9b/2.9c)
- `SAVE SCREEN` / `RESTORE SCREEN` — deferred to 5.6 (separate infrastructure)

---

## 3. Robustness & Stress Testing — COMPLETED

All five stress test areas have been implemented and pass. Tests live in
`tests/test_stress_*.txt` with companion `.PRG` files and `.expected` output.

### ~~3.1 Stack Depth / Recursion Limits~~ TESTED

**Finding: UDF recursion crashes at depth 6 due to SLOW-32 host stack overflow.**

Each recursive UDF call creates nested C stack frames (`prog_run → expr_eval →
prog_udf_call → prog_run`), consuming ~207K of host stack per level. At depth 6
the write hits protected memory (`0x0ffde1dc`). The dBase-level `MAX_CALL_DEPTH=32`
check never triggers because the host stack overflows first.

- Recursive `FACT(5)` = 120 — works (depth 5)
- Recursive `FIB(5)` = 5 — works (max depth ~5)
- Iterative `12!` = 479001600 — works (no stack depth issue)
- Iterative `FIB(20)` = 6765 — works
- 10-deep nested FOR loops (1024 iterations) — works

**Workaround**: Use iterative algorithms for depth > 5. **Root cause fix** requires
either increasing the SLOW-32 stack size at link time or moving large local
allocations (e.g. `value_t` arrays in `prog_run`) to the heap.

Test: `test_stress_recursion.txt` + `STRESS_RECURSE.PRG` + `STRESS_LOOPS.PRG`

### ~~3.2 Large Database Stress~~ TESTED (1000 records)

Tested with 1000 records (reduced from planned 10,000 to keep test runtime under
2 seconds). All operations work correctly:

- Bulk INSERT 1000 records with computed fields — works
- COUNT = 1000, SUM QTY = 49500, AVERAGE QTY = 49.5 — correct
- INDEX ON 1000 records, SEEK specific/missing keys — works
- DELETE FOR (batch, 500 deleted), PACK (500 remaining) — works
- SEEK after PACK — index correctly rebuilt
- SORT TO new database, verify order — works

Test: `test_stress_bigdb.txt` + `STRESS_BIGDB.PRG`

### ~~3.3 String Boundary Conditions~~ TESTED

**Findings — inconsistent truncation behavior across functions:**

| Function | Max output | Behavior |
|----------|-----------|----------|
| `REPLICATE("X", 300)` | 254 chars | Off-by-one: loop uses `pos + slen < sizeof(buf) - 1` |
| `SPACE(300)` | 255 chars | Correct clamp to 255 |
| `PADR/PADL/PADC("X", 300)` | 255 chars | Correct clamp to 255 |
| `str1 + str2` (total ≥ 256) | `LEN(str1)` | Right operand **entirely dropped**, not truncated |
| `SUBSTR(long, 250, 20)` | 6 chars | Correct (returns chars 250–255) |
| `REPLICATE("AB", 200)` | 254 chars | Multi-char unit, same off-by-one limit |

The concatenation drop behavior is surprising but consistent: `if (len + strlen(right)
< sizeof(result->str))` in `expr.c` rejects the entire right operand when the total
would reach 256 or more. The REPLICATE off-by-one (254 vs 255) is in `func.c`.
Neither is a bug per se — both are defensive truncation — but REPLICATE's limit
differs from SPACE/PAD by one character.

Test: `test_stress_strings.txt`

### ~~3.4 Index Integrity Under Mutation~~ TESTED

All index operations maintain integrity correctly:

- REPLACE that changes key order: index stays valid after `SET ORDER TO 1`
  (Note: must use `SET ORDER TO 0` for physical traversal during REPLACE to
  avoid the classic dBASE gotcha of skipping records when traversing by index)
- Multiple indexes with SET ORDER switching — both stay current
- UNIQUE index: duplicate APPEND adds the record but SEEK finds the first
  matching entry (VAL=1, not VAL=4). UNIQUE REPLACE that would create a
  duplicate is rejected with "Uniqueness violation" message, leaving the
  record unchanged
- DELETE + PACK + REINDEX: all indexes rebuilt correctly, SEEK finds remaining
  records, deleted records correctly absent

Test: `test_stress_index.txt` + `STRESS_INDEX.PRG`

### ~~3.5 Multi-Work-Area Interactions~~ TESTED

All multi-area operations work correctly:

- Same DB in two areas: Area 2 sees modifications from Area 1 (shared file
  state — both areas point to the same underlying file)
- SET RELATION: Master→Detail via indexed key works. SKIP in master
  automatically repositions detail area
- 10 simultaneous work areas: all independently addressable via SELECT,
  cross-area field access via alias→field notation works (A->LABEL from
  area 10)

Note: After `USE`, `current_record=0` — must `GO TOP` before field access works.
This is consistent with dBASE III behavior.

Test: `test_stress_workarea.txt` + `STRESS_WA.PRG`

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
- Negative/zero arguments to FWRITE, FREAD — ~~PADR, PADL, PADC~~ (covered in stress_strings)
- Array boundary conditions: `arr[0]`, `arr[rows+1]`, `arr[-1]`
- ~~Recursion depth: what happens at frame 33?~~ Answered: host stack overflow at depth 6 (§3.1)
- ~~REPLACE on non-UNIQUE index with active UNIQUE index~~ (covered in stress_index)
- STORE to array without index (should error per 585f594, needs test)
- Binary data in FREAD/FWRITE (NUL bytes)
- ~~String overflow: concatenation beyond 255 chars~~ (covered in stress_strings)
- Full variable store (256 vars), then DECLARE array
- CALCULATE with `&macro` expression input
- Memo fields with UNIQUE/index rollback (ensure no `.DBT` growth)

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

---

## 9. February 2026 Code Review

### 9.1 REPLACE Performance & Parsing
`cmd_replace` in `command.c` re-initializes a lexer and re-parses the field list
and expression for *every single record* in the loop. For large databases, this
is a massive overhead. It should be refactored to parse once into a list of
field/AST pairs before the loop.

### 9.2 REPLACE Index Inconsistency
`cmd_replace` uses a manual physical record loop instead of `process_records()`.
This makes it ignore the controlling index, which differs from other record-
oriented commands like `COUNT`, `SUM`, or `LOCATE`.

### 9.3 Hardcoded Date in dbf_create
`dbf_create()` in `dbf.c` hardcodes the file creation year to 26 (2026). It
should use the system `time()` to provide accurate metadata.

### 9.4 dbf_memo_read Wasteful I/O
`dbf_memo_read()` reads the entire memo until a terminator (0x1A), even if the
destination buffer is small (typically 256 bytes). For large memos, this
results in wasted disk I/O. It should stop reading once the buffer is full.

### 9.5 String Concatenation Drop (expr.c)
When string concatenation (`+` or `-`) results in a string longer than 255
chars, the right operand is currently dropped entirely. It should be truncated
to fill the remaining space in the 256-byte `value_t.str` buffer.

### 9.6 REPLICATE Off-by-one (func.c)
`fn_replicate` limits the total length to 254 characters due to a strict
`pos + slen < sizeof(buf) - 1` check. It should allow up to 255 characters
(`pos + slen < sizeof(buf)`).

### 9.7 Recursive prog_run
`prog_run()` in `program.c` contains a recursive call to itself when popping
a frame to continue caller execution. This contributes to stack depth issues
unnecessarily. It should be refactored into a single execution loop with a
`goto` or `while` structure.

### 9.8 Unique Violation ON ERROR Bypass
`indexes_update_current` in `command.c` prints a "Uniqueness violation"
message directly to `stdout` instead of using `prog_error()`. This prevents
user programs from trapping the error with `ON ERROR`.

### 9.9 Large Stack Allocations (REPORT FORM)
`cmd_report_form` allocates a 14KB `frm_def_t` on the stack. While safe for
shallow calls, this significantly reduces the headroom for recursive UDFs.
Large definition structs should be moved to the heap or static memory.

### 9.10 AST Constant Folding
The `ast_eval()` engine in `ast.c` currently evaluates all nodes at runtime.
Adding a simple constant folding pass to `ast_compile()` would improve
performance for expressions like `2 + 3` or `YEAR({02/11/26})`.
