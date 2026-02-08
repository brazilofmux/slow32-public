# SLOW BASIC — Post-Implementation Issues (Round 2)

This document tracks bugs and architectural limitations identified in the second review of the sbasic codebase.

## Critical Bugs & Logic Errors

### 1. `fmt_fixed` Overflow and Incorrect Formatting
The `fmt_fixed` helper (used by `PRINT USING`) used `unsigned int` for the integer part of a double.
- **Status**: **FIXED**. Replaced manual digit extraction with `snprintf("%.Xf", val)` which handles any magnitude correctly. Also increased `format_using_num` buffer from 128 to 512 bytes (fixes #11).
- **Verification**: `printusing.bas` tests values > 2^32 (5 billion) and scientific notation.

### 2. Case-Sensitive Identifiers
The reviewer flagged identifiers as case-sensitive.
- **Status**: **NOT A BUG**. The lexer already normalizes all identifiers to uppercase (line 308: `strcpy(tok.text, upper)`). Variables, SUB/FUNCTION names, TYPE names, and field names are all case-insensitive. Verified with mixed-case tests.

### 3. Null-Termination Risks in Fixed-Size Buffers
Multiple constructors in `ast.c` used `strncpy(dst, src, 63)` without explicit null-termination.
- **Status**: **FIXED**. Added `name_copy()` helper that wraps `strncpy` + explicit `dst[63] = '\0'`. All 31 call sites replaced.

### 4. Stack Overflow Risks (C Recursion)
Deeply nested BASIC programs (or very complex expressions) can cause a C stack overflow.
- **Problem**: `stmt_free`, `expr_free`, `eval_stmt`, and `eval_expr` use native C recursion without any depth checks.
- **Recommendation**: Implement a `recursion_depth` counter and limit (e.g., 256) in `eval_expr` and `eval_stmt`. Convert `stmt_free` to use an iterative approach for the `next` chain.

---

## Architectural Limitations & Performance

### 5. O(N²) Statement Appending
The parser uses `stmt_append` which walks the entire statement chain to add a new statement to the end.
- **Problem**: For a 1000-line BASIC program, this results in O(N²) complexity during parsing, which is significantly slower than necessary.
- **Recommendation**: Keep a `tail` pointer in the parser or return head/tail pairs from parsing functions.

### 6. `SWAP` Restricted to Simple Variables
The `STMT_SWAP` implementation and `stmt_swap` constructor only accept variable names.
- **Problem**: Standard BASIC allows `SWAP A(i), A(j)`, but the current implementation cannot handle array elements or record fields.
- **Recommendation**: Refactor `stmt_swap` to take two `expr_t*` nodes (specifically restricted to L-values) instead of string names.

### 7. Fixed-Size Table Overflows (Silent)
The `eval_program` first pass collects labels, procs, and DATA values into fixed-size tables (`MAX_LABELS`, `MAX_PROCS`, `MAX_DATA_VALUES`).
- **Problem**: If these limits are exceeded, the parser/evaluator silently ignores the overflow (it just stops adding to the table).
- **Recommendation**: Return a specific error (e.g., `ERR_OUT_OF_MEMORY` or `ERR_TABLE_FULL`) when these limits are reached.

---

## Language Completeness

### 8. `INPUT` and `READ` Variable Limits
`STMT_INPUT`, `STMT_READ`, and `STMT_INPUT_FILE` use fixed-size arrays of 8 variables.
- **Problem**: Programs attempting to read or input more than 8 variables in a single statement will have the extra variables ignored.
- **Recommendation**: Use a dynamically allocated array for variable names in these statements, similar to how `STMT_PRINT` handles items.

### 9. Lack of `GOSUB` Depth Check on Entry
While `eval_program` has a `MAX_GOSUB_DEPTH` (64) check, it is only checked during execution.
- **Problem**: The parser doesn't know about this limit. While execution-time checking is standard, the limit of 64 might be tight for complex recursive BASIC logic.
- **Recommendation**: Consider increasing the limit or making it dynamic, and ensure the error `ERR_OUT_OF_MEMORY` (used for stack overflow) is descriptive.

### 10. Unchecked Memory Allocations
Many parts of the codebase (`ast.c`, `parser.c`, `builtin.c`, `eval.c`) call `malloc`, `calloc`, `strdup`, or `realloc` without checking for a `NULL` return value.
- **Problem**: In memory-constrained environments (like SLOW-32 with limited heap), this will lead to null-pointer dereferences and crashes rather than a trappable BASIC error.
- **Recommendation**: Audit all allocation sites. In the parser, return `NULL` and set `p->error = ERR_OUT_OF_MEMORY`. In the evaluator, return `ERR_OUT_OF_MEMORY`.

### 11. Fixed-Size Formatting Buffers
In `eval.c`, functions like `format_using_num` and `fmt_fixed` used fixed-size buffers (`char buf[128]`).
- **Status**: **FIXED** (as part of #1). `format_using_num` buffer increased to 512 bytes. `fmt_fixed` uses `snprintf` internally with a 512-byte temp buffer and `dec` clamped to 0..20.
