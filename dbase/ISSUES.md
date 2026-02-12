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

