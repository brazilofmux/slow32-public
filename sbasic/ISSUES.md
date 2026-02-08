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
- **Status**: **FIXED**. Implemented coordinate-aware copy that iterates through shared bounds of all dimensions.

---

## Resource Leaks & Table Limits

### 4. TYPE Record Instance Leaks
`DIM var AS TypeName` appends a new instance to the `type_instances` table in `eval.c` every time it is executed.
- **Status**: **FIXED**. Records are now refcounted `VAL_RECORD` values stored in `env_t`. Heap-allocated and freed when scope exits.

### 5. Global Record Scoping
The `type_instances` table is global and flat.
- **Status**: **FIXED**. Records follow `env_t` scoping. SUB/FUNCTION TYPE vars are local. SHARED works via copy-in/copy-out with shared record pointer.

### 6. Array Table Exhaustion
`array_erase` marks an array as inactive but `array_dim` always appends to the end of the `arrays` table (`array_count++`).
- **Status**: **FIXED**. `array_dim` now scans for `active == 0` slots before appending.

---

## Language Dialect Discrepancies

### 7. `DEFTYPE` Initialization Gap
`DEFINT`, `DEFDBL`, and `DEFSTR` modify a runtime map, but the parser and `env.c` initialization (`create_entry`) currently rely on hardcoded suffix checks.
- **Status**: **FIXED**. Added `env_deftype_hook` callback so `create_entry` consults `deftype_map` for bare names (no suffix). Assignment coercion in `exec_assign` also uses `resolve_var_type()` at runtime instead of the parser's static type.
- **Verification**: `sbasic/tests/deftype.bas` tests DEFINT/DEFSTR ranges, explicit suffix override, and coercion.

### 8. `RND(0)` and `RND(-n)` Logic
- **Problem**: `RND(0)` was generating a new value instead of returning the last. `RND(-n)` was seeding but not returning a value.
- **Status**: **FIXED** in `builtin.c`.
- **Verification**: `sbasic/tests/rnd_test.bas` passes.

### 9. `TAB(n)` Implementation
- **Status**: **FIXED**. Added `print_col` tracking (updated by `col_puts`/`col_printf` wrappers in eval.c). `TAB(n)` now emits just enough spaces to reach absolute column `n` (1-based). If already past column `n`, no spaces are emitted.
- **Verification**: `sbasic/tests/tab.bas` tests absolute positioning, past-target no-op, and multiple TABs per line.

---

## Architectural Improvements

### 10. `EVAL_CHECK` in Blocks
- **Status**: **FIXED**. Audited all `eval_expr`/`eval_stmts` call sites and `val_to_integer`/`val_to_double`/`val_compare` return values. Fixed 8 unchecked coercion calls: array index coercion, LBOUND/UBOUND dimension, FOR loop bounds, RANDOMIZE seed, ERROR n code, ON...GOTO/GOSUB index, and SELECT CASE range comparisons. All block handlers (IF, FOR, WHILE, DO, SELECT, call_proc) correctly propagate error signals.
