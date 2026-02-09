# SLOW BASIC — Post-Implementation Issues (Round 3)

This document tracks bugs and architectural limitations identified in the third review (post-Stage 6) of the sbasic codebase.

## Critical Bugs & Logic Errors

### 1. ~~Constants are mutable via non-assignment statements~~ **FIXED**
Added `env_is_const()` checks to all mutation paths: `INPUT`, `FOR`, `READ`, `INPUT#`, `LINE INPUT#`, `FIELD_ASSIGN`, and `SWAP`. All return `ERR_CONST_REASSIGN` on attempt to modify a `CONST` variable.

### 2. ~~`TAB(n)` and `print_col` are Global to `stdout`~~ **FIXED**
Added per-file column tracking via `active_print_col` indirection pointer. Each `file_handle_t` now has a `col` field. `exec_print_file` redirects `active_print_col` to the file's counter during output, and `fn_tab` reads through the pointer. Stdout column tracking is unchanged.

### 3. ~~Missing Integer Overflow Checks~~ **FIXED**
Integer arithmetic now uses `long long` intermediates. If the result exceeds INT32 range, it auto-promotes to `VAL_DOUBLE`. Applies to `val_add`, `val_sub`, `val_mul`, `val_pow`, and `val_neg` (INT_MIN negation).

### 4. ~~`SHARED` Variable "Copy-in/Copy-out" Race Condition~~ **FIXED**
Added `link` pointer to `var_entry_t`. `SHARED` variables now use `env_link()` to point the local entry directly at the global's `value_t` storage. All reads/writes through `env_get`/`env_set` follow the link. No copy-in or copy-out needed — nested SUB calls see changes immediately.

### 5. ~~`MID$` Statement (L-Value) is Missing~~ **FIXED**
Added `STMT_MID_ASSIGN` AST node. `parse_paren_dispatch` intercepts `MID$` before the generic array-assignment path. `exec_mid_assign` implements QBasic-compatible semantics: `MID$(A$, start [, length]) = rep$` replaces up to `min(length, LEN(rep$))` characters starting at position `start` (1-based). Total string length is preserved.

---

## Language Completeness & Opportunities

### 6. ~~Lexer: Missing Hex/Octal/Binary Literals~~ **FIXED**
Added `&H` (hex), `&O` (octal), and `&B` (binary) literal support in `scan_token`. Bare `&` produces integer 0 (QBasic compat). Optional `%` suffix consumed and ignored.

### 7. ~~`VAL()` doesn't recognize Hex/Octal prefixes~~ **FIXED**
Updated `fn_val` to detect `&H`, `&O`, `&B` prefixes and use `strtol` with the appropriate base (16, 8, 2) before falling through to decimal parsing.

### 8. ~~Extended File I/O: `LOC`, `LOF`, and `INPUT$`~~ **FIXED**
Added `LOC(h)` (current byte position), `LOF(h)` (file length via seek), and `INPUT$(n [, #h])` (read n characters from file or stdin) as builtins. Parser updated to allow decorative `#` in function argument lists (e.g., `INPUT$(5, #1)`).

### 9. ~~System Integration: `DIR$`, `ENVIRON$`, `COMMAND$`~~ **FIXED**
Added `ENVIRON$(name$)` (reads env vars via MMIO getenv), `COMMAND$([n])` (command-line args via `__slow32_fetch_args`), and `DIR$(pattern$)` / `DIR$()` (directory listing via MMIO opendir/readdir with wildcard matching).

### 10. ~~`REPLACE$(str, find, rep)`~~ **FIXED** (already implemented)

### 11. ~~High-Resolution `TIMER`~~ **FIXED**
`TIMER` now calls `time()` + `localtime()` via MMIO and returns seconds since midnight as a double, matching QBasic semantics.