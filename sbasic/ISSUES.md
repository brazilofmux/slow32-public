# SLOW BASIC — Post-Implementation Issues (Round 4)

This document tracks bugs and architectural limitations identified in the fourth review of the sbasic codebase.

## Critical Bugs & Logic Errors

### 1. ~~Infinite Recursion in `ERROR n` Handler~~ **FIXED**
Added `!on_error_active` guard to the `ERR_ON_ERROR_GOTO` handler in `eval_program`. `ERROR n` inside an active error handler now falls through to fatal error instead of recursing.

### 2. ~~Memory Leak: `SHARED` String/Record Variables~~ **FIXED**
`env_link` now calls `val_clear(&e->value)` on the local entry before setting the link pointer, preventing the auto-created default value from leaking.

### 3. ~~Labels, `DATA`, and `SUB` Visibility in Blocks~~ **FIXED**
Replaced the flat top-level scan with a recursive AST walker (`scan_one_stmt`/`scan_stmt_list`) that descends into IF, FOR, WHILE, DO, and SELECT blocks to collect labels, DATA values, and SUB/FUNCTION definitions regardless of nesting level.

### 4. `GOTO/GOSUB` cannot target nested blocks
The `pc` (program counter) in `eval_program` is an index into the top-level `stmts` array.
- **Problem**: Even if labels were collected from nested blocks, the `eval_program` loop has no way to jump *into* a block (like the middle of an `IF` statement). Conversely, a `GOTO` from inside a block to a label in the *same* block will jump to the top-level `pc` target, potentially breaking the control flow.
- **Recommendation**: This is an architectural limitation of the current "flattened top-level" execution model. While jumping *into* blocks is often discouraged, jumping *within* or *out of* blocks should work correctly.

### 5. ~~`eval_depth` (32) is too shallow~~ **FIXED**
Bumped `MAX_EVAL_DEPTH` from 32 to 48 and stack size from 64KB to 128KB (via `--stack-size 128K` linker flag). Each recursion level uses ~2KB of C stack, so 48 levels needs ~96KB — safely within 128KB.

---

## Architectural Limitations & Performance

### 6. ~~Missing Range Checks for `INTEGER` Conversion~~ **FIXED**
`val_to_integer` now checks doubles against INT32 range before truncating, returning `ERR_OVERFLOW` if out of bounds. `fn_cint` similarly checks after rounding.

### 7. ~~Fixed Limit on `SHARED` variables~~ **FIXED**
Bumped `shared_names` array from 32 to 64 entries.

---

## Language Completeness & Opportunities

### 8. Binary/Random File I/O (`GET`, `PUT`, `FIELD`)
- **Opportunity**: Support for `OPEN "file" FOR RANDOM / BINARY` and the associated `GET #h, pos, var` and `PUT #h, pos, var`.
- **Context**: Essential for database-like applications or processing non-text files.

### 9. `STATIC` Variables in Procedures
- **Opportunity**: The `STATIC` keyword for `SUB/FUNCTION` variables to preserve their value between calls.
- **Recommendation**: Add a `static_env` to the `proc_entry_t` that is reused across calls.

### 10. `INKEY$` and ~~`SLEEP`~~
- **`SLEEP [n]`**: **FIXED** — `SLEEP n` pauses execution for n seconds (uses MMIO `sleep()`). `SLEEP` or `SLEEP 0` is a no-op.
- **`INKEY$`**: Requires non-blocking stdin input and terminal raw mode — needs new MMIO opcode support.
