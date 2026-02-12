# dBase III Clone: Open Issues & Future Work

This is a *living* planning document. Completed items are summarized at the end.

## 1. Open Bugs (Correctness)

### 1.1 ~~CALCULATE expression capture breaks with macro expansion~~ — RESOLVED
Fixed: expression text is now captured by scanning the raw original input string
(paren-balanced from `token_start+1` to matching `)`) instead of using pointer
arithmetic on `token_start`/`l->p`, which break when the multi-character operator
lookahead in `lex_next` triggers macro expansion as a side effect.
The callback also uses `ast_eval_dynamic()` for consistent macro re-expansion.

## 2. Behavior Regressions / Architecture Follow-ups

These are potential behavior shifts introduced by AST-based evaluation and
lexer changes. Confirm against dBase III expectations.

### 2.1 ~~Macro expansion timing~~ — RESOLVED
Expressions containing `&` now skip AST caching and recompile fresh on every
evaluation via `ast_eval_dynamic()`, matching dBase III/FoxPro semantics.

### 2.2 Array access resolution time
`name(...)` becomes array access *only if* the array exists at compile time.
If the array is declared later, the AST remains a function call.

### 2.3 ~~AST function-arg parsing limits~~ — RESOLVED
`ast_parse_primary()` now enforces `MAX_FUNC_ARGS` at compile time and handles
`realloc` failure with proper cleanup and "Out of memory" error.

## 3. Polish & Completeness (Open)

### 3.1 Error handling completeness
- `ON ERROR` handler exists but some error paths bypass it (e.g., UNIQUE
  violations print directly to stdout)
- No `ERROR()` code for UNIQUE violations, file handle exhaustion, etc.
- No `RETRY` support for recoverable errors in data entry

### 3.2 LIST STRUCTURE TO filename
`LIST STRUCTURE` (or `COPY STRUCTURE EXTENDED`) creates a DBF containing the
field definitions of the current database. Used for dynamic schema operations.

### 3.3 SAVE SCREEN / RESTORE SCREEN
Save and restore terminal contents. Used for popup dialogs and help screens.

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

