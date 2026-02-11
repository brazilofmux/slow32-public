# dBase III Clone: Open Issues & Future Work

## 1. Known Bugs

### 1.1 FWRITE negative count: out-of-bounds read
`func.c` — `FWRITE(h, "hello", -1)` passes negative `count` to `fwrite()`,
which casts to a huge `size_t` and reads far past the string buffer. Fix: clamp
`count` to zero if negative.

### 1.2 Array dimension overflow
`memvar.c` — `DECLARE arr[100000, 100000]` overflows `rows * cols` on 32-bit
int. The `calloc` allocates a tiny buffer but dimension metadata stays large,
so element access overwrites random memory. Fix: cap total elements (e.g., 8192)
and reject oversized declarations.

### 1.3 VALID rejection leaves dirty memvar
`screen.c` — When VALID evaluates to `.F.`, the code has already written the
rejected value into the memvar store before checking. On re-prompt or user
abort, the invalid value persists. Fix: save the old value before the set and
restore it on VALID failure.

### 1.4 ADIR pattern filter non-functional
`func.c` — `ADIR("*.PRG")` returns the same count as `ADIR()` with no
filter. The `str_like` function uses SQL-style `%`/`_` wildcards, but users
pass DOS-style `*`/`?` patterns. The test expected-output was updated to match
the broken count, masking the bug. Fix: either translate `*`/`?` to `%`/`_`
before calling `str_like`, or add a proper glob matcher.

### 1.5 FREAD silently truncates to 255 bytes
`func.c` — `FREAD(h, 1000)` reads 1000 bytes into a malloc'd buffer, but
`val_str()` copies into `value_t.str[256]`. Data is silently truncated. Fix:
cap the read count at 255 (sizeof `value_t.str` - 1).

### 1.6 Memory leak on full variable store
`memvar.c` — `memvar_declare_array` allocates the array struct and elements
via `calloc`, then calls `memvar_set`. If the store is full, `memvar_set`
returns -1 but the allocations are never freed. `h_declare` in command.c
doesn't check the return value either.

### 1.7 FOPEN/FCREATE don't set ll_error when handle table full
`func.c` — When all 16 file handle slots are occupied, `FOPEN()`/`FCREATE()`
return -1 but don't set `ll_error`. `FERROR()` may report 0 (success).

### 1.8 Duplicate FILE entry in func_table
`func.c` — `{ "FILE", fn_file }` appears twice (directory services section and
misc section). The second is dead code. Remove the duplicate.

### 1.9 VALID/WHEN expressions evaluated at parse time
`screen.c` — To capture the expression text for later evaluation, the parser
evaluates the expression into a dummy result just to advance the pointer. Side
effects (function calls, record navigation) fire at `@GET` time instead of
READ time. If the eval fails, the captured expression is empty/truncated,
silently disabling validation.

### 1.10 WHEN/VALID silently pass on eval errors
`screen.c` — If `expr_eval_str` returns non-zero (expression syntax error) or
the result is not `VAL_LOGIC` (e.g., returns a number), validation is silently
skipped. A typo in a VALID expression effectively disables the constraint.

### 1.11 Term-mode READ ignores VALID and WHEN
`screen.c` — The `#if HAS_TERM` full-screen READ path only checks RANGE. The
VALID and WHEN clauses are only honored in the fallback line-mode path.

### 1.12 Bracket string syntax broken
`lex.c` — `[` is now tokenized as `TOK_LBRACKET` for array indexing. In
dBase III, `[Hello]` is an alternative string literal (equivalent to `"Hello"`).
Any code using bracket-delimited strings breaks silently. Need context-sensitive
lexing: `[` after an identifier = array index; `[` at expression start = string.

### 1.13 ensure_dbf_ext uppercases directory path
`command.c` — `ensure_dbf_ext()` calls `str_upper()` on the entire path,
including directory components. On case-sensitive Linux filesystems,
`data/myfile` becomes `DATA/MYFILE.DBF`, which won't open. Should uppercase
only the filename portion.

---

## 2. Compatibility Gaps

These are features a dBase III/Clipper/FoxPro programmer would expect.

### 2.1 Memo Fields (.DBT)
The `M` field type (variable-length text stored in a companion `.DBT` file) is
not supported. This is the single biggest format gap. Memo fields are used for
notes, descriptions, long text — any field > 254 characters. Without memo
support, importing real-world `.DBF` files with memo fields will fail or lose
data.

- dBase III: 512-byte block-based `.DBT` file
- FoxPro: variable-block `.FPT` file
- At minimum, support read-only access to memo fields in existing files

### 2.2 EDIT/BROWSE — Full-Screen Record Editor
`EDIT` and `BROWSE` are the primary interactive data entry commands in dBase.
`BROWSE` displays a spreadsheet-style grid of records with scrolling,
field-by-field editing, and insert/delete. This is a large feature (several
hundred lines), but it's the command most dBase users reach for first.

- Requires terminal mode (already available via `term.h`)
- Core operations: scroll up/down, edit field in place, append, delete
- Clipper extended BROWSE with code blocks for custom behavior

### 2.3 INKEY()/LASTKEY()/READKEY() — Keyboard Input
Currently stubbed to return 0 or 13. Real dBase programs use `INKEY()` to wait
for a keypress (with optional timeout), `LASTKEY()` to check what key exited a
READ, and `READKEY()` to determine how the user left the last field.

- Essential for menu-driven applications
- `INKEY(0)` = wait forever; `INKEY(5)` = wait 5 seconds; `INKEY()` = poll
- Key codes: 27=Esc, 13=Enter, 5=Up, 24=Down, 19=Left, 4=Right, etc.
- Terminal key reading infrastructure already exists in `term.h`

### 2.4 TEXT...ENDTEXT — Block Text Output
Outputs literal text without interpretation. Used for report headers, help
screens, boilerplate. Every line between `TEXT` and `ENDTEXT` is printed
verbatim. Simple to implement but frequently used.

### 2.5 SCATTER/GATHER — Record-to-Memvar Transfer
`SCATTER` copies the current record's fields into memory variables (one per
field, named after the field). `GATHER` writes them back. This is the standard
pattern for editing a record in a form and then committing changes. Without it,
users must write individual `REPLACE` commands for every field.

### 2.6 SET Commands — Missing Options
Several commonly-used SET options are not implemented:

| SET | Purpose | Priority |
|-----|---------|----------|
| SET TALK ON/OFF | Suppress/show command feedback (record counts, etc.) | High |
| SET ECHO ON/OFF | Show/hide command lines during DO | Medium |
| SET HEADING ON/OFF | Column headers in LIST/DISPLAY | Medium |
| SET SAFETY ON/OFF | Prompt before overwriting files | Medium |
| SET BELL ON/OFF | Beep on input errors | Low |
| SET CENTURY ON/OFF | 2-digit vs 4-digit year display | Medium |
| SET DATE | Date display format (AMERICAN, BRITISH, etc.) | Medium |
| SET DECIMALS | Default decimal places | Medium |
| SET MARGIN | Left margin for printer output | Low |
| SET ESCAPE ON/OFF | Allow Esc to interrupt programs | Medium |

### 2.7 ON KEY — Event-Driven Key Handling
`ON KEY = <key> <command>` triggers a command when a specific key is pressed.
Used for function-key menus and hotkeys. Clipper extended this with `SET KEY`
and code blocks.

### 2.8 CALCULATE — Aggregate Expressions
`CALCULATE` computes multiple aggregate values in a single pass:
```
CALCULATE AVG(salary), MAX(salary), MIN(salary), CNT() TO avg_s, max_s, min_s, cnt
```
More flexible than individual SUM/AVERAGE/COUNT commands.

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

### 4.1 Abstract Syntax Tree (AST)
Currently, expressions are parsed and evaluated in a single recursive-descent
pass. An AST would separate parsing from evaluation, enabling:

- **Repeated evaluation without re-parsing**: FOR/WHILE conditions, VALID/WHEN
  expressions, and index key expressions are all re-evaluated per-record. Today,
  each evaluation re-lexes and re-parses the expression string. An AST would
  parse once and walk the tree on each evaluation.
- **Better error messages**: AST nodes can carry source positions.
- **Foundation for optimization**: constant folding, common subexpression
  elimination, dead branch pruning.

**Trade-offs**: Adds ~300-500 lines of AST node types and tree-walk evaluator.
Increases memory usage (one AST per live expression). On SLOW-32, the parsing
overhead may be small relative to I/O. Profile before committing.

**Verdict**: Worth it for correctness (eliminates the VALID/WHEN parse-time
side-effect bug from 1.9) and for index key evaluation performance on large
databases. Not urgent for small workloads.

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

### 5.1 ADIR() with DOS-style Wildcards
Currently broken (see 1.4). Once fixed, should support standard patterns:
`*.DBF`, `REP*.FRM`, `?DATA.DBF`. This is table stakes for directory operations.

### 5.2 Error Handling Completeness
- `ON ERROR` handler exists but some error paths bypass it (e.g., UNIQUE
  violation prints directly to stdout instead of routing through `prog_error`)
- No `ERROR()` code for UNIQUE violations, file handle exhaustion, etc.
- No `RETRY` support for recoverable errors in data entry

### 5.3 DISPLAY MEMORY
`DISPLAY MEMORY` shows all active memory variables, their types, values, and
scope (PUBLIC/PRIVATE). Essential for debugging. Currently not implemented.

### 5.4 DISPLAY STATUS
`DISPLAY STATUS` shows open databases, active indexes, relations, filter
expressions, and SET options. The dBase programmer's "what's going on?" command.

### 5.5 LIST STRUCTURE TO filename
`LIST STRUCTURE` (or `COPY STRUCTURE EXTENDED`) creates a DBF containing the
field definitions of the current database. Used for dynamic schema operations.

### 5.6 SAVE SCREEN / RESTORE SCREEN
Save and restore the terminal contents. Used for popup dialogs and help screens
that overlay the main display and then restore it.

---

## 6. Test Coverage Gaps

### 6.1 Tests That Mask Bugs
- `test_dir_services.expected` — ADIR pattern count matches broken output (1.4)
- `test_get_validation` — only tests line-mode; term-mode untested (1.11)

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
