# SLOW BASIC — Post-Implementation Issues (Round 3)

This document tracks bugs and architectural limitations identified in the third review (post-Stage 6) of the sbasic codebase.

## Critical Bugs & Logic Errors

### 1. ~~Constants are mutable via non-assignment statements~~ **FIXED**
Added `env_is_const()` checks to all mutation paths: `INPUT`, `FOR`, `READ`, `INPUT#`, `LINE INPUT#`, `FIELD_ASSIGN`, and `SWAP`. All return `ERR_CONST_REASSIGN` on attempt to modify a `CONST` variable.

### 2. `TAB(n)` and `print_col` are Global to `stdout`
The `print_col` variable is a single global integer.
- **Problem**: When using `PRINT #1, TAB(10); "text"`, the interpreter uses the cursor position of the console (`stdout`) to calculate the required spaces for the file. This results in incorrect formatting in files.
- **Recommendation**: Move `print_col` into the `file_handle_t` structure so each file (and `stdout`) tracks its own column position.

### 3. Missing Integer Overflow Checks
Integer arithmetic (`VAL_INTEGER`) uses native C operators.
- **Problem**: `val_add`, `val_sub`, `val_mul`, and `val_pow` do not check for overflow. On SLOW-32, `30000 * 30000` will overflow silently. Standard BASIC typically promotes the result to a `DOUBLE` or throws an `OVERFLOW` error.
- **Recommendation**: Use overflow-aware arithmetic (or check bounds before operation) and either promote to `VAL_DOUBLE` or return `ERR_OVERFLOW`.

### 4. `SHARED` Variable "Copy-in/Copy-out" Race Condition
`SHARED` variables are copied to local scope on `SUB` entry and back to global on exit.
- **Problem**: If a global variable is modified while a `SUB` is active (e.g., by a nested call or an `ON ERROR` handler that modifies global state), the `SUB`'s local copy will overwrite the updated global value when the `SUB` returns. This is not true "shared" storage.
- **Recommendation**: Implement true variable linking in `env.c` where a local `var_entry_t` can point to the storage of a global entry.

### 5. `MID$` Statement (L-Value) is Missing
Standard BASIC supports `MID$(A$, start [, length]) = "newtext"`.
- **Problem**: The interpreter only supports `MID$` as a function. Attempting to use it as a statement will likely be misparsed as an array assignment or a syntax error.
- **Recommendation**: Update `parse_stmt` to recognize `MID$(` as an assignment target and implement `exec_mid_assign`.

---

## Language Completeness & Opportunities

### 6. Lexer: Missing Hex/Octal/Binary Literals
- **Opportunity**: Add support for `&HFF` (Hex), `&O77` (Octal), and `&B1010` (Binary). 
- **Recommendation**: Update `scan_token` in `lexer.c` to handle the `&` prefix.

### 7. `VAL()` doesn't recognize Hex/Octal prefixes
- **Problem**: `VAL("&HFF")` currently returns 0 because it uses `atof`.
- **Recommendation**: Implement a custom numeric parser for `fn_val` that understands BASIC's numeric prefixes.

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