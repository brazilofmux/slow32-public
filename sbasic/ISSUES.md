# SLOW BASIC — Post-Implementation Issues (Round 4)

This document tracks bugs and architectural limitations identified in the fourth review of the sbasic codebase.

## Critical Bugs & Logic Errors

### 1. ~~Infinite Recursion in `ERROR n` Handler~~ **FIXED**
Added `!on_error_active` guard to the `ERR_ON_ERROR_GOTO` handler in `eval_program`. `ERROR n` inside an active error handler now falls through to fatal error instead of recursing.

### 2. ~~Memory Leak: `SHARED` String/Record Variables~~ **FIXED**
`env_link` now calls `val_clear(&e->value)` on the local entry before setting the link pointer, preventing the auto-created default value from leaking.

### 3. Labels, `DATA`, and `SUB` Visibility in Blocks
The first-pass scan in `eval_program` iterates only over the top-level `stmts` array.
- **Problem**: `DATA` statements, line labels, and `SUB/FUNCTION` definitions located inside `IF...END IF`, `FOR...NEXT`, or other blocks are completely ignored. They are not collected into the global tables and thus cannot be used.
- **Recommendation**: Implement a recursive scanner that walks the entire AST to collect labels, DATA, and procedures regardless of nesting level.

### 4. `GOTO/GOSUB` cannot target nested blocks
The `pc` (program counter) in `eval_program` is an index into the top-level `stmts` array.
- **Problem**: Even if labels were collected from nested blocks, the `eval_program` loop has no way to jump *into* a block (like the middle of an `IF` statement). Conversely, a `GOTO` from inside a block to a label in the *same* block will jump to the top-level `pc` target, potentially breaking the control flow.
- **Recommendation**: This is an architectural limitation of the current "flattened top-level" execution model. While jumping *into* blocks is often discouraged, jumping *within* or *out of* blocks should work correctly.

### 5. `eval_depth` (32) is too shallow
The limit for C recursion is currently set to 32.
- **Problem**: Since every nested block (`IF`, `FOR`, `WHILE`, `DO`, `SELECT`) and every `SUB/FUNCTION` call increments this depth, complex BASIC programs can easily hit this limit and trigger a `Stack overflow` error.
- **Recommendation**: Increase `MAX_EVAL_DEPTH` to 128 or 256. The SLOW-32 stack is large enough to handle this.

---

## Architectural Limitations & Performance

### 6. Missing Range Checks for `INTEGER` Conversion
Functions like `fn_cint` and `val_to_integer` (used for array indices, `TAB`, etc.) do not check for 32-bit overflow.
- **Problem**: Converting a very large `DOUBLE` to `INTEGER` results in silent wrapping or undefined behavior, which can lead to illegal memory access (e.g., in array indexing) or logic errors.
- **Recommendation**: Add range checks to `val_as_int` and return `ERR_OVERFLOW` or `ERR_ILLEGAL_FUNCTION_CALL`.

### 7. Fixed Limit on `SHARED` variables
`call_proc` uses a fixed-size array `shared_names[32]` to track variables to be linked.
- **Problem**: Procedures using more than 32 `SHARED` variables will silently fail to link the extras.
- **Recommendation**: Use a dynamically allocated list or a higher limit (e.g. 128).

---

## Language Completeness & Opportunities

### 8. Binary/Random File I/O (`GET`, `PUT`, `FIELD`)
- **Opportunity**: Support for `OPEN "file" FOR RANDOM / BINARY` and the associated `GET #h, pos, var` and `PUT #h, pos, var`.
- **Context**: Essential for database-like applications or processing non-text files.

### 9. `STATIC` Variables in Procedures
- **Opportunity**: The `STATIC` keyword for `SUB/FUNCTION` variables to preserve their value between calls.
- **Recommendation**: Add a `static_env` to the `proc_entry_t` that is reused across calls.

### 10. `INKEY$` and `SLEEP`
- **Opportunity**: `INKEY$` for non-blocking keyboard input and `SLEEP [n]` for pausing.
- **Context**: High value for interactive programs and games.
