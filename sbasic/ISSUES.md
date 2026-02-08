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

### 8. Extended File I/O: `LOC`, `LOF`, and `INPUT$`
- **Opportunity**: Add `LOC(h)` (current position), `LOF(h)` (length of file), and `INPUT$(n, [#h])` (read $n$ characters).
- **Context**: These are essential for robust binary file processing and structured data handling.

### 9. System Integration: `FILES`, `ENVIRON$`, `COMMAND$`
- **Opportunity**: 
    - `FILES` (or `DIR$`): List files in the current directory.
    - `ENVIRON$(name)`: Read environment variables.
    - `COMMAND$`: Get the command-line arguments passed to the interpreter.
- **Context**: The SLOW-32 MMIO runtime already provides these hooks; they just need BASIC-level wrappers.

### 10. `REPLACE$(str, find, rep)`
- **Status**: **Already implemented** in `builtin.c` (`fn_replace`). Registered in the dispatch table as `REPLACE$`.

### 11. High-Resolution `TIMER`
- **Problem**: The current `TIMER` returns a hardcoded `val_integer(0)` — there is no clock implementation at all, not even `time(NULL)`.
- **Recommendation**: Use `gettimeofday` or a cycles-based counter (if available in the emulator) to provide a meaningful timer value.