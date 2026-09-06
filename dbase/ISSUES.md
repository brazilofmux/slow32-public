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

