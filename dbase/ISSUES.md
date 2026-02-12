# dBase III Clone: Open Issues & Future Work

This is a *living* planning document. Completed items are summarized at the end.

## 1. Open Bugs (Correctness)

### 1.1 CALCULATE expression capture breaks with macro expansion
`command.c` — `cmd_calculate()` captures expression text via `token_start` while
the lexer advances. With macro expansion, `token_start` can point into the macro
stack buffer and be invalidated, leading to corrupted or truncated expressions.

**Suggested fix:** Copy the raw expression text into a stable buffer before the
lexer advances, or parse into AST directly.

## 2. Behavior Regressions / Architecture Follow-ups

These are potential behavior shifts introduced by AST-based evaluation and
lexer changes. Confirm against dBase III expectations.

### 2.1 ~~Macro expansion timing~~ — RESOLVED
Expressions containing `&` now skip AST caching and recompile fresh on every
evaluation via `ast_eval_dynamic()`, matching dBase III/FoxPro semantics.

### 2.2 Array access resolution time
`name(...)` becomes array access *only if* the array exists at compile time.
If the array is declared later, the AST remains a function call.

### 2.3 AST function-arg parsing limits
`ast_parse_primary()` defers `MAX_FUNC_ARGS` enforcement to eval-time and lacks
`realloc` failure handling. Very long arg lists can consume memory or crash.

**Suggested fix:** Enforce `MAX_FUNC_ARGS` at compile time and handle `realloc`
failure by reporting "Out of memory" and freeing partial args.

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

### 4.1 Missing scenarios
- Negative/zero args to FWRITE/FREAD
- Array boundary conditions: `arr[0]`, `arr[rows+1]`, `arr[-1]`
- STORE to array without index (should error per 585f594)
- Binary data in FREAD/FWRITE (NUL bytes)
- Full variable store (256 vars), then DECLARE array
- CALCULATE with `&macro` expression input

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

