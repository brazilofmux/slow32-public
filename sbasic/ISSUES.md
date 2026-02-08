# SLOW BASIC — Post-Implementation Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the Stage 6 "feature complete" interpreter.

## Critical Bugs

### 1. Incomplete Error Trapping (eval.c)
The `ON ERROR GOTO` logic in `eval_program` used a range check (`err < ERR_EXIT`) that excluded all errors added in Stages 3-6 (e.g., Subscript out of range, File not found, Undefined TYPE).
- **Status**: **FIXED**. Replaced range check with `is_trappable_error()` helper that explicitly identifies flow-control signals vs real errors.
- **Verification**: `sbasic/tests/error_test.bas` successfully traps `File not found (39)`.

### 2. RESUME Signal Propagation
`RESUME` and `RESUME NEXT` return `ERR_RESUME_WITHOUT_ERROR` as a signal to the main `eval_program` loop. However, if `RESUME` is called inside a block (like an `IF...END IF` or `FOR...NEXT`), the old code checked `stmts[pc]->type == STMT_RESUME` to determine the variant, but `pc` pointed to the enclosing block, not the RESUME statement.
- **Status**: **FIXED**. RESUME now sets a `resume_is_next` global flag before returning its signal. The `eval_program` handler uses this flag instead of inspecting `stmts[pc]`.

### 3. Incorrect `REDIM PRESERVE` for Multi-dimensional Arrays
In `array_redim`, the code uses a flat `val_copy` from the old data array to the new one.
- **Problem**: This is only correct for 1D arrays. For a 2D array, if the number of columns changes, the flat index mapping of every row shifts. A `memcpy` of the old flat buffer into the new one will scramble the data.
- **Recommendation**: Implement a coordinate-aware copy that iterates through the shared bounds of all dimensions.

---

## Resource Leaks & Table Limits

### 4. TYPE Record Instance Leaks
`DIM var AS TypeName` appends a new instance to the `type_instances` table in `eval.c` every time it is executed.
- **Problem**: In recursive functions or loops, `MAX_TYPE_INSTANCES` (128) is easily exhausted.
- **Recommendation**: Integrate record instances into the `env_t` scope system or implement slot reuse/name-lookup for re-dimensioning.

### 5. Global Record Scoping
The `type_instances` table is global and flat. 
- **Problem**: Record variables do not follow BASIC's local scoping rules for `SUB/FUNCTION`. A record variable in a `SUB` will collide with one of the same name in another `SUB`.
- **Recommendation**: Move record data storage into the `env_t` environment.

### 6. Array Table Exhaustion
`array_erase` marks an array as inactive but `array_dim` always appends to the end of the `arrays` table (`array_count++`).
- **Problem**: Repeatedly `DIM`ing and `ERASE`ing (common in some BASIC patterns) will eventually hit `MAX_ARRAYS` (64).
- **Recommendation**: `array_dim` should scan for `active == 0` slots before appending.

---

## Language Dialect Discrepancies

### 7. `DEFTYPE` Initialization Gap
`DEFINT`, `DEFDBL`, and `DEFSTR` modify a runtime map, but the parser and `env.c` initialization (`create_entry`) currently rely on hardcoded suffix checks.
- **Problem**: `DEFINT A-Z` should make a bare variable `A` an integer, but it currently defaults to `VAL_DOUBLE`.
- **Recommendation**: `env_get` and `create_entry` should consult the `deftype_map` when creating a variable without an explicit suffix.

### 8. `RND(0)` and `RND(-n)` Logic
- **Problem**: `RND(0)` was generating a new value instead of returning the last. `RND(-n)` was seeding but not returning a value.
- **Status**: **FIXED** in `builtin.c`.
- **Verification**: `sbasic/tests/rnd_test.bas` passes.

### 9. `TAB(n)` Implementation
- **Problem**: Currently implemented as `SPACE$(n)`.
- **Recommendation**: The interpreter needs to track the current cursor column (print position) and emit enough spaces to reach absolute column `n`.

---

## Architectural Improvements

### 10. `EVAL_CHECK` in Blocks
Ensure that every block execution (like `SELECT CASE`) is wrapped in logic that can propagate signals. While most have been audited, any new Stage 6 features should be double-checked for "silent" error swallowing.
