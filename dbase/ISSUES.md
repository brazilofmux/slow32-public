# dBase III Clone: Open Issues & Future Work

This is a *living* planning document. Completed items are summarized at the end.

## 1. Open Bugs (Correctness)

### 1.1 ~~CALCULATE expression capture breaks with macro expansion~~ — RESOLVED
Fixed: expression text is now captured by scanning the raw original input string
(paren-balanced from `token_start+1` to matching `)`) instead of using pointer
arithmetic on `token_start`/`l->p`, which break when the multi-character operator
lookahead in `lex_next` triggers macro expansion as a side effect.
The callback also uses `ast_eval_dynamic()` for consistent macro re-expansion.

### 1.2 String Concatenation Drop
`expr.c` — When string concatenation (`+` or `-`) results in a string longer
than 255 chars, the right operand is currently dropped entirely. It should
be truncated to fill the remaining space in the 256-byte `value_t.str` buffer.

### 1.3 Hardcoded Date in dbf_create
`dbf.c` — `dbf_create()` hardcodes the file creation year to 26 (2026). It
should use the system `time()` to provide accurate metadata.

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

### 2.4 REPLACE Performance & Parsing
`command.c` — `cmd_replace()` re-initializes a lexer and re-parses the field list
and expression for *every single record* in the loop. For large databases, this
is a massive overhead. It should be refactored to parse once into a list of
field/AST pairs before the loop.

### 2.5 REPLACE Index Inconsistency
`command.c` — `cmd_replace()` uses a manual physical record loop instead of
`process_records()`. This makes it ignore the controlling index, which differs
from other record-oriented commands like `COUNT`, `SUM`, or `LOCATE`.

### 2.6 Recursive prog_run
`program.c` — `prog_run()` contains a recursive call to itself when popping a
frame to continue caller execution. This contributes to stack depth issues
unnecessarily. It should be refactored into a single execution loop with a
`goto` or `while` structure.

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

### 3.3 SAVE SCREEN / RESTORE SCREEN
Save and restore terminal contents. Used for popup dialogs and help screens.

### 3.4 REPLICATE Off-by-one
`func.c` — `fn_replicate` limits the total length to 254 characters due to a
strict `pos + slen < sizeof(buf) - 1` check. It should allow up to 255
characters (`pos + slen < sizeof(buf)`).

### 3.5 dbf_memo_read Wasteful I/O
`dbf.c` — `dbf_memo_read()` reads the entire memo until a terminator (0x1A),
even if the destination buffer is small (typically 256 bytes). For large
memos, this results in wasted disk I/O. It should stop reading once the
buffer is full.

### 3.6 Large Stack Allocations (REPORT FORM)
`command.c` — `cmd_report_form` allocates a 14KB `frm_def_t` on the stack.
While safe for shallow calls, this significantly reduces the headroom for
recursive UDFs. Large definition structs should be moved to the heap or
static memory.

### 3.7 AST Constant Folding
`ast.c` — The `ast_eval()` engine currently evaluates all nodes at runtime.
Adding a simple constant folding pass to `ast_compile()` would improve
performance for expressions like `2 + 3` or `YEAR({02/11/26})`.

## 4. Test Coverage Gaps

### 4.1 Missing scenarios — MOSTLY RESOLVED
Added tests: `test_edge_fio` (FWRITE/FREAD zero/negative/invalid),
`test_edge_arrays` (boundary conditions, STORE without index),
`test_edge_varstore` (full store + DECLARE), `test_edge_calc_macro`
(CALCULATE with `&macro` expressions, nested parens).
Remaining: binary data in FREAD/FWRITE (NUL bytes truncate due to string
representation — known limitation, not testable via dBase strings).

### 4.2 Fragile tests
- `test_dir_services` embeds absolute path and file count — breaks if repo moves
  or files are added/removed. Should be restructured to test relative behavior.

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

