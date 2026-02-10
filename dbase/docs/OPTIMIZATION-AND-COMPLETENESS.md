# Optimization and Completeness Plan

To transform the current dBase III clone into a "non-toy" implementation capable of running legacy software like "Teacher's Pet", several architectural and feature-level improvements are required.

## 1. Architectural: Lexer & Parser

The current ad-hoc string scanning in `expr.c`, `command.c`, and `program.c` is fragile and difficult to extend.

- **Lexical Stage**: Introduce a proper lexer (`lex.c`) that tokenizes the input stream.
  - Support for dBase macros (`&var`) at the lexer level.
  - Case-insensitive keyword matching with the **4-character abbreviation rule** (any command or keyword can be shortened to its first 4 characters).
  - Proper handling of string literals, numeric constants, and logical literals (`.T.`).
- **Recursive Descent Parser**: Rebuild the command and expression parsers on top of the lexer.
  - This will allow for more complex command syntax (e.g., `REPLACE` with multiple fields, scopes, and conditions).
  - Improved error reporting with column-specific error messages.

## 2. Performance: B-Tree Indexing

The current in-memory sorted array for indexes is a major bottleneck for large databases.

- **On-Disk B-Tree**: Implement a standard dBase-compatible `.NDX` format using 512-byte pages.
- **Lazy Loading**: Only keep the necessary pages in memory.
- **O(log N) Performance**: Ensure `SEEK` and `FIND` remain fast even as record counts grow.
- **Automatic Updates**: Ensure `APPEND` and `REPLACE` correctly update all open indexes.

## 3. Completeness: Language Features

To support existing dBase III codebases, the following features must be fleshed out:

### Environment & Control
- **`SET PROCEDURE TO <file>`**: Implement procedure libraries for code reuse.
- **`RELEASE ALL [LIKE/EXCEPT <pattern>]`**: Implement wildcard-based memory variable management.
- **Missing Functions**: Implement `ROW()`, `COL()`, `PCOL()`, `PROW()`, `DISKSPACE()`, `VERSION()`, `OS()`.
- **`SET` Commands**: Implement missing environment toggles used in legacy code:
  - `SET ECHO`, `SET MENU`, `SET STATUS`, `SET SCOREBOARD`, `SET FUNCTION`, `SET HELP`, `SET PROCEDURE`.
- **`PRIVATE` and `PUBLIC`**: Improve scope management for variables.

### Data Manipulation
- **`REPLACE` enhancements**: Support `[scope]` and `[FOR <cond>]` on `REPLACE`.
- **`JOIN`**: Merge two databases based on a common field.
- **`UPDATE`**: Update a database from another database.
- **`TOTAL`**: Create a summary database with totals.
- **`SORT` enhancements**: Support multiple keys and descending order.

### Output & UI
- **Report Generator**: Full implementation of `REPORT FORM` using `.FRM` files.
- **Label Generator**: Implementation of `LABEL FORM` using `.LBL` files.
- **Terminal UI**: Improve `@ SAY/GET` with better `PICTURE` mask support and high-intensity color handling (`W+`).

## 4. Verification: "Teacher's Pet" Compatibility

The ultimate test of completeness is running the legacy "Teacher's Pet" (`tp.prg`) application.

- **Macro substitution**: Verify all cases of `&` macro expansion.
- **Procedure calls**: Ensure nested `DO` and `PARAMETERS` work correctly with the new parser.
- **Date handling**: Verify `DATE()` and date arithmetic are robust.
- **Screen handling**: Ensure the terminal UI correctly renders the menus and forms.
