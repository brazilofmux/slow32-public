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
### 1.3a Reserved Keyword Enforcement Coverage
- **Status**: Reserved keyword checks are now enforced in `STORE`, direct assignments, `PARAMETERS`, `PRIVATE`, `PUBLIC`, and interactive `ACCEPT`/`INPUT`/`WAIT` targets.
- **Note**: This keeps dBase III/Clipper behavior tight without introducing additional macro indirection.

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

- **Reserved Keywords**: Established a core reserved keyword list. Assignments to command names are now caught at parse-time, ensuring unambiguous procedure and control-flow detection.
- **Transparent Macro Expansion**: Integrated a robust, multi-level macro expansion engine directly into the Lexer via a recursive character-streaming stack.
- **Lexer Clause Refactor**: Migrated all record-level sequential commands to a unified Lexer-based parser and a generic `process_records` engine.
- **Work Area Registry**: Centralized work area management in `area.c` with robust, token-aware alias resolution (A-J, 1-10) across all commands, including `SET RELATION`.
- **Commit 97a8b75**: Resolved regressions in `LIST`/`DISPLAY` output redirection (`TO FILE`), added robust filename parsing for quoted paths, and ensured proper restoration of record pointers.
- **Commit eb41794**: Implemented `RANGE` validation in full-screen terminal mode.
