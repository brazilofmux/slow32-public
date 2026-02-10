# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities. Organized by theme, with observations from recent implementation phases.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration (Ongoing)
- **Status**: Major sub-handlers (`LIST`, `DISPLAY`, `REPLACE`, `REPORT FORM`, `COUNT`, `SUM`, `AVERAGE`, `DELETE`, `RECALL`, `LOCATE`, `CONTINUE`) have been refactored to use the Lexer and a unified `clause_t` structure.
- **Opportunity**: Transition remaining command dispatchers (e.g., `STORE`, `RELEASE`, `SET`) to the Lexer for 100% architectural consistency.

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
- **Issue**: Each `dbf_t` handle has its own 32-record read-ahead cache.
- **Bug**: If the same DBF file is opened in two work areas, writing to one does not invalidate the cache of the other.
- **Strategy**: Implement a shared global record cache indexed by file inode/device, or document this as a known limitation of the multi-area implementation.

### 2.3 SORT External Merge Sort
- **Status**: `cmd_sort` has been refactored into a multi-pass external merge sort.
- **Milestone**: Supports sorting databases of any size (up to 128,000 records with current constants) by using temporary disk chunks and a K-way merge.

### 2.4 Index Build Performance
- **Status**: `index_build` now uses a bottom-up strategy. Keys are collected, sorted via library `qsort_r`, and packed into leaves.
- **Milestone**: Achieves O(N) build time (excluding sort), producing perfectly packed B+ trees with minimal I/O.

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

- **Function Argument Scaling**: Increased the maximum number of arguments for function calls and UDFs to 32. This allows for much more complex programming patterns and data-entry helpers.
- **External Merge Sort**: Refactored `SORT` to use a multi-pass external merge sort algorithm. It sorts data in manageable chunks, writes them to disk, and merges them using a K-way merge. This allows the system to sort databases far larger than the available RAM.
- **O(N) Index Build**: Implemented bottom-up index construction. By sorting keys in memory first and packing the B+ tree from the leaves up, we eliminate the O(N log N) individual insertion overhead and produce perfectly balanced, highly efficient index files.
- **qsort_r in Runtime**: Extended the SLOW-32 C runtime library with `qsort_r` (reentrant quicksort), enabling context-aware sorting across the entire platform.
- **Sparse Page Table**: Refactored the B+ Tree engine to use a hash-based sparse page table. This eliminates the O(N) memory scaling of metadata, making the indexer capable of handling extremely large datasets without proportional RAM growth.
- **Reserved Keywords**: Established a core reserved keyword list. Assignments to command names are now caught at parse-time, ensuring unambiguous procedure and control-flow detection.
- **Transparent Macro Expansion**: Integrated a robust, multi-level macro expansion engine directly into the Lexer via a recursive character-streaming stack.
- **Lexer Clause Refactor**: Migrated all record-level sequential commands to a unified Lexer-based parser and a generic `process_records` engine.
- **Work Area Registry**: Centralized work area management in `area.c` with robust, token-aware alias resolution (A-J, 1-10) across all commands, including `SET RELATION`.
- **Commit eb41794**: Implemented `RANGE` validation in full-screen terminal mode.
