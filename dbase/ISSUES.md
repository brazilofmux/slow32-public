# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities. Organized by theme, with observations from recent implementation phases.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration
- **Status**: **Completed.** All primary command dispatchers (`STORE`, `RELEASE`, `SET`, `USE`, `ERASE`, `RENAME`, `ACCEPT`, `INPUT`, `WAIT`, `SAVE`, `RESTORE`, etc.) and major sequential sub-handlers have been refactored to use the Lexer and a unified architecture.
- **Milestone**: Achieved 100% architectural consistency for command parsing.

### 1.2 Unified Macro Handling
- **Status**: Transparent, multi-level macro expansion (`&var`) has been integrated directly into the Lexer and the `process_records` engine. This replaces the brittle pre-processing stage.
- **Milestone**: Supports nested macros and robust argument expansion across all commands.

### 1.3 Procedure Search Robustness
- **Status**: Core keywords (`PROCEDURE`, `FUNCTION`, `IF`, `DO`, etc.) are now reserved words. Assignments to these names are rejected.
- **Milestone**: `find_procedure` is now 100% robust against false positives in assignments, while `prog_preprocess` continues to handle comments and strings.

### 1.4 Function Argument Scaling
- **Status**: `MAX_FUNC_ARGS` constant (32) has been implemented and unified across `expr.c` and `program.c`.
- **Milestone**: UDFs and built-in functions can now accept up to 32 arguments, removing the previous limit of 8/16.

## 2. Performance & Scaling

### 2.1 B+ Tree Sparse Page Table
- **Status**: The linear `pages` array has been replaced with a fixed-size hash table (`page_hash`).
- **Milestone**: Metadata overhead no longer scales with index file size. Supports multi-gigabyte indexes with constant-time lookup and minimal RAM footprint.

### 2.2 DBF Cache Coherence
- **Status**: Global cache invalidation is now implemented across all work areas.
- **Milestone**: Writing to a DBF in one work area automatically invalidates the read-ahead cache of any other work areas that have the same file open, ensuring data consistency.

### 2.3 SORT External Merge Sort
- **Status**: `cmd_sort` has been refactored into a multi-pass external merge sort.
- **Milestone**: Supports sorting databases of any size (up to 128,000 records with current constants) by using temporary disk chunks and a K-way merge.

### 2.4 Index Build Performance
- **Status**: `index_build` now uses a bottom-up strategy. Keys are collected, sorted via library `qsort_r`, and packed into leaves.
- **Milestone**: Achieves O(N) build time (excluding sort), producing perfectly packed B+ trees with minimal I/O.

### 2.5 B+ Tree Maintenance (Free List & Merging)
- **Status**: **Completed.** `index_remove` now implements full B+ tree underflow handling (borrowing from and merging with siblings) and page reclamation via a persistent free list in the NDX2 header.
- **Milestone**: Indices remain compact and efficient even after heavy deletion activity. Space from deleted nodes is immediately reusable.

## 3. Correctness & Fidelity

### 3.1 Printer Logic & PROW()/PCOL()
- **Status**: Automatic page eject is now implemented in `@SAY` (when requested row < `PROW()`). `?` and `??` now correctly update printer coordinates.
- **Milestone**: Printer state is tracked accurately across all output commands, ensuring correct pagination and alignment in reports.

### 3.2 Fractional Exponents
- **Status**: Exponentiation (`**` and `^`) now uses the standard library `pow()` function.
- **Milestone**: Supports fractional powers (e.g., `9 ^ 0.5` = 3), expanding the engine's mathematical capabilities beyond simple integers.

### 3.3 Error Message Thread Safety
- **Status**: **Completed.** `expr_ctx_t` now features a dedicated `err_msg` buffer. Formatted errors in `expr.c` and `func.c` no longer use static buffers, preventing corruption during nested execution.
- **Milestone**: Achieved robust, re-entrant error reporting across the entire expression and function engine.

## 4. Future Opportunities

### 4.1 [Feature] Memory Variable Arrays
- **Opportunity**: Extend `memvar.c` and the Lexer to support one and two-dimensional arrays (e.g., `DECLARE arr[10]`, `STORE "val" TO arr[5]`).
- **Benefit**: Essential for complex data manipulation and parity with Clipper/FoxPro. Requires updating `memvar_store_t` to handle indexed lookups.

### 4.2 [Architecture] Nested UDF Calls in Expressions
- **Opportunity**: Enhance `expr.c` and `func.c` to allow User-Defined Functions defined in `.PRG` files to be called directly within expressions (e.g., `LIST name, MyBonus(salary)`).
- **Benefit**: Dramatically increases the power of reports and data updates. Requires careful handling of the call stack during expression evaluation.

### 4.3 [Feature] Low-Level File I/O (Clipper Parity)
- **Status**: **Completed.** Implemented `FCREATE()`, `FOPEN()`, `FREAD()`, `FWRITE()`, `FSEEK()`, `FCLOSE()`, and `FERROR()` with a fixed handle table.
- **Milestone**: `FILEIO.PRG` regression coverage verifies basic create/open/read/write/seek/close flows and error reporting.

### 4.4 [Feature] Standard Function Parity
- **Opportunity**: Implement missing core dBase functions: `DTOC()`, `CTOD()`, `TRANSFORM()`.
- **Benefit**: Essential for string manipulation and data formatting parity with Clipper and FoxPro.

## 5. Completed Milestone Successes

- **SCAN / ENDSCAN**: Implemented record iteration with `FOR`, `WHILE`, and scope clauses.
- **UNIQUE Indexes**: Added `UNIQUE` indexes and enforced constraints on `REPLACE`.
- **GET Validation**: Implemented `VALID` and `WHEN` clauses for `@ ... GET`.
- **Program Command Lexing**: Refactored `PARAMETERS`, `PRIVATE`, `PUBLIC`, and `PROCEDURE` to use the Lexer.
- **Date Arithmetic**: Added date addition/subtraction in the expression engine.
- **Directory Services**: Implemented `ADIR()`, `FILE()`, and `CURDIR()`.
- **Low-Level File I/O**: Implemented `FCREATE()`, `FOPEN()`, `FREAD()`, `FWRITE()`, `FSEEK()`, `FCLOSE()`, and `FERROR()`.
- **Standard Function Expansion**: Implemented `LEFT()`, `RIGHT()`, `ALLTRIM()`, `EMPTY()`, and the `PADx()` family.
- **Cross-Platform Paths**: Normalized path separators in file operations and index/dbf I/O.


