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

## 5. Future Opportunities

### 5.1 [Feature] SCAN / ENDSCAN Iteration
- **Opportunity**: Implement the `SCAN [scope] [FOR cond] [WHILE cond]` command. 
- **Benefit**: Provides a much faster and more idiomatic way to iterate over records compared to manual `DO WHILE .NOT. EOF()` loops. It automatically handles record pointer advancement and restoration.

### 5.2 [Feature] Memory Variable Arrays
- **Opportunity**: Extend `memvar.c` and the Lexer to support one and two-dimensional arrays (e.g., `DECLARE arr[10]`, `STORE "val" TO arr[5]`).
- **Benefit**: Essential for complex data manipulation and parity with Clipper/FoxPro. Requires updating `memvar_store_t` to handle indexed lookups.

### 5.3 [Architecture] Nested UDF Calls in Expressions
- **Opportunity**: Enhance `expr.c` and `func.c` to allow User-Defined Functions defined in `.PRG` files to be called directly within expressions (e.g., `LIST name, MyBonus(salary)`).
- **Benefit**: Dramatically increases the power of reports and data updates. Requires careful handling of the call stack during expression evaluation.

### 5.4 [Harden] @ ... GET Validation (VALID/WHEN)

- **Opportunity**: Implement `VALID <expr>` and `WHEN <expr>` clauses for the `@ ... GET` command in `screen.c`.

- **Benefit**: Enables proactive data integrity checks and conditional field entry in full-screen modes, fulfilling a core requirement for professional dBase applications.



### 5.5 [Feature] UNIQUE Indexes

- **Opportunity**: Add support for the `UNIQUE` keyword in the `INDEX ON` command.

- **Benefit**: Essential for data integrity and standard dBase compatibility. Requires adding a `unique` flag to `index_t` and updating `index_insert` to handle duplicates correctly.



### 5.6 [Feature] Low-Level File I/O (Clipper Parity)

- **Opportunity**: Implement `FOPEN()`, `FREAD()`, `FWRITE()`, `FSEEK()`, and `FCLOSE()`.

- **Benefit**: Allows dBase programs to manipulate non-DBF files (logs, exports, etc.), a major feature of professional Clipper and FoxPro applications.



### 5.7 [Correctness] Date Arithmetic Parity

- **Opportunity**: Fully implement date addition/subtraction (e.g., `DATE() + 30`, `d1 - d2`).

- **Benefit**: Core requirement for business logic. Requires updating the expression engine's `TOK_PLUS` and `TOK_MINUS` handlers to detect date types.



### 5.8 [Feature] ADIR() and Directory Services



- **Opportunity**: Add `ADIR()`, `FILE()`, and `CURDIR()` functions.



- **Benefit**: Allows programs to be file-system aware, which is necessary for robust application installers and data importers.







### 5.9 [Feature] Standard Function Parity



- **Opportunity**: Implement missing core dBase functions: `LEFT()`, `RIGHT()`, `DTOC()`, `CTOD()`, `TRANSFORM()`, `EMPTY()`, and `SPACE()`.



- **Benefit**: Essential for string manipulation and data formatting parity with Clipper and FoxPro.







### 5.10 [Architecture] Lexer Integration (Program commands)



- **Opportunity**: Transition `PARAMETERS`, `PRIVATE`, `PUBLIC`, and `PROCEDURE` handlers in `program.c` to use the Lexer.



- **Benefit**: Removes remaining brittle string-parsing logic and ensures consistent macro expansion and tokenization across the entire engine.







### 5.11 [Harden] Cross-Platform Path Handling



- **Opportunity**: Normalize paths in `dbf_open` and `index_read` (e.g., converting backslashes to forward slashes).



- **Benefit**: Improves compatibility when running legacy dBase programs that hardcode DOS-style paths.




