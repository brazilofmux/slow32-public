# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities. Organized by theme, with observations from recent implementation phases.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration (Medium Priority)
While `src/expr.c` correctly uses the new Lexer for expression parsing, `src/command.c` still relies on manual string scanning and `str_imatch`.
- **Observation**: Commands are not consistently parsed across the system. The "4-character rule" is implemented in `lex.c` but not utilized for command dispatch in `command.c`.
- **Note**: `program.c` recently adopted specialized identifier scanning (`line_is_kw`) for control-flow robustness, which is a step toward consistency without the full overhead of the Lexer for every line scan.
- **Opportunity**: Refactor `cmd_execute` to use the Lexer. This will simplify parsing of complex commands (e.g., `REPORT FORM` or `COPY TO`) which have multiple optional clauses.

### 1.2 Unified Macro Handling
Macro expansion (`&var`) is currently handled in a pre-processing stage (`prog_preprocess`).
- **Issue**: This can lead to double-tokenization issues or failures in complex nested expressions.
- **Opportunity**: Integrate macro expansion directly into `lex.c` so that the Lexer can transparently expand macros as it tokenizes.

## 2. Performance & Scaling

### 2.1 B+ Tree Memory Residency
The Phase 7 implementation correctly moved the system to a professional **B+ Tree (NDX2 format)** with 4KB pages.
- **Success**: Incremental maintenance during `APPEND`, `REPLACE`, `PACK`, and `ZAP` is excellent.
- **Observation**: Currently, all pages are loaded into memory (`pages` array). For extremely large databases, this will consume significant RAM.
- **Strategy**: The current memory-resident approach is ideal for "giving the structure a workout" on the SLOW-32 platform. A future pass should implement a true **LRU Page Cache** (e.g., 16–64 pages) to support multi-gigabyte indexes.

### 2.2 Record Buffering
`src/dbf.c` reads records one at a time directly from disk into a single buffer per work area.
- **Limitation**: Sequential scans (LIST, COUNT, etc.) are bottlenecked by disk I/O latency.
- **Opportunity**: Implement a simple LRU record cache or read-ahead buffering.

## 3. Correctness & Compatibility

### 3.1 Wildcard Matching
`memvar.c`'s `pattern_match` currently only supports trailing `*` (e.g., `T_*`).
- **Issue**: Standard dBase III patterns like `?` or mid-string `*` are not fully supported.
- **Opportunity**: Implement a robust `str_like` function in `util.c`.

### 3.2 Printer Logic in @SAY
`src/screen.c` `@SAY` in `PRINT` mode advances `print_row` using `\n`.
- **Issue**: If a report attempts to print at a row *less* than the current `print_row`, it currently does nothing.
- **Note**: Authentic dBase behavior varies (some issue a Form Feed, others ignore). This should be verified against Teacher's Pet requirements.

## 4. Completed Milestone Successes

- **Phase 5 (Error Handling)**: Successfully transitioned from `printf` errors to a robust `prog_error()` system with typed codes and `ON ERROR` recovery.
- **Phase 6 (Binary Compatibility)**: `.FRM` and `.LBL` formats are precisely implemented at the binary level, matching legacy specifications.
- **Phase 7 (B+ Tree)**: Moved from O(N) to O(log N) indexing with a stable, persistent tree structure and backward-compatible NDX1 loader.
- **Commit 4fe3f75**: Fixed `SAVE TO` variable iteration (skipping unused slots) and implemented a keyword-aware parser for `REPORT FORM` to allow clauses after `FOR/HEADING`.
- **Commit 05639a9/40bcb2e**: Integrated identifier-aware scanning for control flow keywords (`IF`, `ENDDO`, etc.), preventing confusion from keywords embedded in strings or comments.
