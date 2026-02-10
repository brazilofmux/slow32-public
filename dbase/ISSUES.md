# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration (Ongoing)
- **Success**: `cmd_execute` in `src/command.c` now utilizes the Lexer for primary command dispatch.
- **Observation**: Many sub-handlers (e.g., `cmd_list`, `cmd_replace`, `cmd_report_form`) still perform manual string scanning for clauses (`FOR`, `WHILE`, `TO PRINT`).
- **Opportunity**: Fully transition sub-handlers to use the Lexer. This will resolve "keyword swallowing" bugs where a clause like `FOR` consumes the rest of the line, preventing subsequent clauses.

### 1.2 Unified Macro Handling
- **Issue**: Macro expansion (`&var`) is handled by `prog_preprocess` before the Lexer sees the line.
- **Limitation**: This prevents the Lexer from handling nested macros or macros that expand into partial tokens.
- **Opportunity**: Integrate macro expansion directly into `lex.c`. When the Lexer encounters `&`, it should look up the variable and continue tokenizing from the expanded string.

### 1.3 Procedure Search Robustness
- **Issue**: `find_procedure` in `program.c` uses `str_imatch` on raw source lines.
- **Bug**: It can incorrectly identify a `PROCEDURE` keyword inside a comment or a string literal.
- **Opportunity**: Update `find_procedure` to use `line_is_kw` (which pre-processes the line) to ensure only valid code is matched.

## 2. Performance & Scaling

### 2.1 B+ Tree LRU Page Cache
- **Success**: Implementation of a 64-page LRU cache with pinning logic.
- **Observation**: The `pages` array in `index_t` is still sized to the total number of pages in the file. While pointers are NULL for non-resident pages, this array grows with the file size.
- **Opportunity**: Transition to a fixed-size hash table or a more sparse structure for the page table to support multi-gigabyte indexes with minimal overhead.

### 2.2 DBF Cache Coherence
- **Issue**: Each `dbf_t` handle has its own 32-record read-ahead cache.
- **Bug**: If the same DBF file is opened in two work areas, writing to one does not invalidate the cache of the other.
- **Strategy**: Implement a shared global record cache indexed by file inode/device, or document this as a known limitation of the multi-area implementation.

### 2.3 SORT Memory Limitation
- **Limitation**: `cmd_sort` is hard-coded to a maximum of 2000 records.
- **Opportunity**: Implement an external merge sort (sorting chunks to temporary files and merging) to support sorting databases of any size.

## 3. Correctness & Compatibility

### 3.1 Wildcard Matching
- **Issue**: `memvar.c`'s `pattern_match` currently only supports trailing `*` (e.g., `T_*`).
- **Opportunity**: Implement a robust `str_like` function in `util.c` that handles `?` and internal `*` for full dBase III compatibility.

### 3.2 Terminal Mode RANGE Validation
- **Bug**: `screen_read` correctly validates `RANGE` in fallback line-mode but **ignores it** in full-screen terminal mode.
- **Impact**: User input in terminal mode can bypass range constraints defined in `@ GET`.

### 3.3 Screen Position Synchronization
- **Issue**: `screen_say` and `screen_get` update `scr.last_row/col` manually.
- **Concern**: If the terminal service issues its own wraps or scrolling, the internal tracking will drift.
- **Opportunity**: Query the terminal service for actual cursor position if the API supports it, or implement strict bounds checking.

### 3.4 Numeric Formatting in STR()
- **Issue**: `fn_str` in `func.c` currently truncates fractional values to `int` when decimals are not specified.
- **Correction**: Standard dBase `STR()` behavior is to round to the nearest integer if no decimal count is provided.

### 3.5 Fractional Exponents
- **Limitation**: `my_pow` in `expr.c` only supports integer exponents.
- **Opportunity**: Use `pow()` from the runtime library or implement a series-based approximation to support fractional powers (e.g., `x ** 0.5`).

## 4. Completed Milestone Successes

- **Phase 5 (Error Handling)**: Robust `prog_error()` system with `ON ERROR` and `RETRY` support.
- **Phase 6 (Binary Compatibility)**: Precise `.FRM` and `.LBL` binary support.
- **Phase 7 (B+ Tree)**: Persistent NDX2 format with O(log N) performance and incremental maintenance.
- **Record Cache**: 32-record read-ahead window in `dbf.c` for sequential scan acceleration.
- **Lexer Dispatch**: Consistent command identification using the 4-character rule in `command.c`.
