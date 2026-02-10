# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities. Organized by theme, with observations from recent implementation phases.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration (Ongoing)
- **Status**: Major sub-handlers (`LIST`, `DISPLAY`, `REPLACE`, `REPORT FORM`) have been refactored to use the Lexer and a unified `clause_t` structure.
- **Opportunity**: Continue transitioning remaining sub-handlers (e.g., `COUNT`, `SUM`, `AVERAGE`, `DELETE`, `RECALL`, `LOCATE`) to the Lexer for full consistency.

### 1.2 Unified Macro Handling
- **Issue**: Macro expansion (`&var`) is handled by `prog_preprocess` before the Lexer sees the line.
- **Limitation**: This prevents the Lexer from handling nested macros or macros that expand into partial tokens.
- **Opportunity**: Integrate macro expansion directly into `lex.c`. When the Lexer encounters `&`, it should look up the variable and continue tokenizing from the expanded string.

### 1.3 Procedure Search Robustness
- **Issue**: `find_procedure` in `program.c` uses `str_imatch` on raw source lines.
- **Bug**: It can incorrectly identify a `PROCEDURE` keyword inside a comment or a string literal.
- **Opportunity**: Update `find_procedure` to use `line_is_kw` (which pre-processes the line) to ensure only valid code is matched.

### 1.4 Function Argument Scaling
- **Limitation**: `parse_primary` uses a fixed-size `args[8]` array for function calls.
- **Opportunity**: Transition to dynamic allocation or a larger buffer to support functions with more than 8 arguments.

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

### 2.4 Index Build Performance
- **Issue**: `index_build` is O(N log N) because it performs individual `index_insert` calls for every record.
- **Opportunity**: Implement a bottom-up build strategy (sort keys first, then pack leaves) to achieve O(N) build time.

### 2.5 B+ Tree Maintenance (Fragmentation)
- **Issue**: `index_remove` unlinks empty pages but does not perform underflow merging or redistribution.
- **Impact**: Heavy deletion activity results in index fragmentation ("dead space") that is only recoverable via `REINDEX`.

## 3. Correctness & Fidelity

### 3.1 Printer Logic in @SAY
- **Issue**: `src/screen.c` `@SAY` in `PRINT` mode advances `print_row` using `\n`.
- **Observation**: If a report attempts to print at a row *less* than the current `print_row`, it currently does nothing.
- **Note**: Authentic dBase behavior varies (some issue a Form Feed, others ignore). This should be verified against Teacher's Pet requirements.

### 3.2 Fractional Exponents
- **Limitation**: `my_pow` in `expr.c` only supports integer exponents.
- **Opportunity**: Use `pow()` from the runtime library or implement a series-based approximation to support fractional powers (e.g., `x ** 0.5`).

### 3.3 Error Message Thread Safety
- **Issue**: `expr.c` and `program.c` frequently use `static char errbuf[]` for error messages.
- **Concern**: While the current platform is single-threaded, this pattern is brittle and can lead to message corruption if an error handler itself triggers another error.

## 4. Completed Milestone Successes

- **Phase 5 (Error Handling)**: Robust `prog_error()` system with `ON ERROR` and `RETRY` support.
- **Phase 6 (Binary Compatibility)**: Precise `.FRM` and `.LBL` binary support.
- **Phase 7 (B+ Tree)**: Persistent NDX2 format with O(log N) performance and incremental maintenance.
- **Record Cache**: 32-record read-ahead window in `dbf.c` for sequential scan acceleration.
- **Lexer Dispatch**: Consistent command identification using the 4-character rule in `command.c`.
- **Work Area Registry**: Centralized work area management in `area.c` with robust, token-aware alias resolution (A-J, 1-10) across all commands, including `SET RELATION`.
- **Lexer Clause Parsing**: Sub-handlers for `LIST`, `DISPLAY`, `REPLACE`, and `REPORT FORM` now use the central Lexer and a unified `clause_t` structure, resolving keyword swallowing bugs and supporting arbitrary clause ordering.
- **Commit eb41794**: Implemented `RANGE` validation in full-screen terminal mode, ensuring data integrity during user input.
- **Commit f9bc96c**: Enhanced screen cursor tracking (`ROW()`/`COL()`) to correctly handle 80-column line wrapping.
- **Commit 81a0d85**: Corrected `STR()` behavior to round values to the nearest integer when no decimal count is specified.
- **Commit d33019e**: Implemented robust, non-trailing wildcard matching (`*` and `?`) for memory variable patterns and general string utility.