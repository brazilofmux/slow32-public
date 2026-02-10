# dBase III Clone: Code Review Issues & Opportunities

This document tracks identified bugs, architectural inconsistencies, and performance opportunities. Organized by theme, with observations from recent implementation phases.

## 1. Architectural Inconsistencies

### 1.1 Lexer Integration (High Priority)
While `src/expr.c` correctly uses the new Lexer for expression parsing, `src/command.c` and `src/program.c` still rely on manual string scanning and `str_imatch`.
- **Bug**: Commands are not consistently parsed. The "4-character rule" is implemented in `lex.c` but not utilized for command dispatch in `command.c`.
- **Opportunity**: Refactor `cmd_execute` and `prog_execute_line` to use the Lexer. This will simplify parsing of complex commands (e.g., `REPORT FORM` or `COPY TO`) which have multiple optional clauses.

### 1.2 Unified Macro Handling
Macro expansion (`&var`) is currently handled in a pre-processing stage (`prog_preprocess`).
- **Issue**: This can lead to double-tokenization issues or failures in complex nested expressions.
- **Opportunity**: Integrate macro expansion directly into `lex.c` so that the Lexer can transparently expand macros as it tokenizes.

### 1.3 Control Flow Robustness
`program.c` uses simple line-scanning for `IF/ELSE/ENDIF` and `DO WHILE`.
- **Issue**: It can be confused by keywords inside string literals or comments.
- **Opportunity**: Use the Lexer during control-flow scanning to ensure only valid tokens are considered.

## 2. Performance & Scaling

### 2.1 B+ Tree Memory Residency
The Phase 7 implementation correctly moved the system to a professional **B+ Tree (NDX2 format)** with 4KB pages.
- **Success**: Incremental maintenance during `APPEND`, `REPLACE`, `PACK`, and `ZAP` is excellent.
- **Observation**: Currently, all pages are loaded into memory (`pages` array). For extremely large databases, this will consume significant RAM.
- **Strategy**: As discussed, the current memory-resident approach is ideal for "giving the structure a workout" on the SLOW-32 platform. A future pass should implement a true **LRU Page Cache** (e.g., 16–64 pages) to support multi-gigabyte indexes.

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

### 3.3 Specific Bugs Identified
- **`command.c` (SAVE TO)**: The memory variable loop iterates `memvar_store.count` times but doesn't skip unused slots in the `vars` array. It should iterate `MEMVAR_MAX` and check `used`.
- **`command.c` (REPORT FORM)**: The `HEADING` and `FOR` clauses currently consume the "rest of the line." This prevents users from putting keywords like `PLAIN` or `SUMMARY` *after* a `FOR` condition. (Requires Lexer integration to fix properly).

## 4. Completed Milestone Successes

- **Phase 5 (Error Handling)**: Successfully transitioned from `printf` errors to a robust `prog_error()` system with typed codes and `ON ERROR` recovery.
- **Phase 6 (Binary Compatibility)**: `.FRM` and `.LBL` formats are precisely implemented at the binary level, matching legacy specifications.
- **Phase 7 (B+ Tree)**: Moved from O(N) to O(log N) indexing with a stable, persistent tree structure and backward-compatible NDX1 loader.

## 5. Recent Review Findings (Phase 7 B+ Tree)

### 5.1 Internal Node Routing on Separator Equality (High Priority)
`index.c` uses `split_key = first key of right page` when splitting a leaf, which implies internal separators represent the first key in the right child. But descent uses `page_lower_bound` and then selects `children[pos]`, which routes `key == separator` to the left child. That leaf does not contain the key after a split, so `SEEK` and inserts can be mis-routed at leaf boundaries.
- **Files**: `dbase/src/index.c` (`bt_find_leaf`, `index_insert`, `leaf_split`, `internal_insert_at`)
- **Fix options**:
  - Use `upper_bound` semantics for descent.
  - Or keep `lower_bound` and route to `children[pos + 1]` on equality.
  - Or redefine separators to be "max key of left child" and adjust split key accordingly.

### 5.2 `SEEK` Exact Match Misses Next Leaf
If `SEEK` lands past the end of a leaf, it advances the iterator to `next_leaf` but does not check for an exact match there. This combines badly with the separator-equality routing above and can return "not found" even when the key exists in the next leaf.
- **Files**: `dbase/src/index.c` (`index_seek`)
- **Fix**: If `next_leaf` exists, compare `key` to its first key and return exact match if equal.

### 5.3 Key Length Derived from First Record Only
`index_build` sizes `key_len` by evaluating the expression on record 1. If later records produce longer outputs, keys are truncated and ordering/collisions can become wrong (especially for expression-based indexes).
- **Files**: `dbase/src/index.c` (`index_build`)
- **Fix**: Derive width from expression metadata when possible, or compute the max across records (or a representative sample).
