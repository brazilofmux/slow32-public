# dBase III Clone — Implementation Status

Current as of February 2026. 52/52 tests passing.

## Architecture

```
src/main.c       — REPL loop, banner
src/command.c    — Command dispatch (str_imatch pattern), ~75 commands
src/expr.c       — Expression evaluator (lexer-based recursive descent)
src/lex.c        — Tokenizer (identifiers, strings, numbers, dates, operators)
src/func.c       — 39 built-in functions
src/dbf.c        — DBF file format read/write
src/field.c      — Field type helpers (C/N/D/L)
src/memvar.c     — Memory variable store (256 vars)
src/set.c        — SET option management
src/program.c    — PRG file execution, control flow, procedures, functions
src/screen.c     — @SAY/@GET/READ, boxes, CLEAR, SET COLOR
src/index.c      — NDX index files (B+ tree, 4KB pages, NDX2 format)
src/report.c     — FRM report definitions and report generation
src/label.c      — LBL label definitions and label generation
src/date.c       — Date arithmetic and formatting
src/util.c       — String helpers (str_imatch, trim, skip_ws)
```

Build: `cd dbase && bash build.sh`
Test:  `bash tests/run-tests.sh`

## Commands Implemented

### Database & File Management
| Command | Syntax |
|---------|--------|
| CREATE | `CREATE <file>` (interactive field definition) |
| CREATE REPORT | `CREATE REPORT <file>` (interactive report definition) |
| CREATE LABEL | `CREATE LABEL <file>` (interactive label definition) |
| USE | `USE [<file>] [INDEX <ndx1>[,<ndx2>...]] [ALIAS <name>]` |
| CLOSE | `CLOSE [DATABASES\|ALL\|INDEX\|PROCEDURE]` |
| SELECT | `SELECT <n>` or `SELECT <alias>` (work areas 1-10) |
| ERASE | `ERASE <file>` |
| RENAME | `RENAME <old> TO <new>` |
| COPY FILE | `COPY FILE <src> TO <dst>` |
| COPY TO | `COPY TO <file> [FIELDS <list>] [<scope>] [FOR <cond>]` |
| COPY STRUCTURE | `COPY STRUCTURE TO <file>` |
| APPEND FROM | `APPEND FROM <file>` |
| SORT | `SORT TO <file> ON <field>[/A\|/D\|/C] [<scope>] [FOR <cond>]` |

### Record Navigation
| Command | Syntax |
|---------|--------|
| GO / GOTO | `GO <n>`, `GO TOP`, `GO BOTTOM` |
| SKIP | `SKIP [<n>]` (default +1, negative ok) |
| LOCATE | `LOCATE [<scope>] FOR <cond>` |
| CONTINUE | `CONTINUE` |
| SEEK | `SEEK <expr>` (binary search in active index) |
| FIND | `FIND <literal>` |

### Record Editing
| Command | Syntax |
|---------|--------|
| APPEND BLANK | `APPEND BLANK` |
| REPLACE | `REPLACE [<scope>] <field> WITH <expr> [,<field> WITH <expr>...] [FOR <cond>]` |
| DELETE | `DELETE [<scope>] [FOR <cond>]` |
| RECALL | `RECALL [<scope>] [FOR <cond>]` |
| PACK | `PACK` |
| ZAP | `ZAP` |

### Display & Output
| Command | Syntax |
|---------|--------|
| LIST | `LIST [<scope>] [FIELDS <list>] [FOR <cond>] [OFF] [TO PRINT\|TO FILE <f>]` |
| DISPLAY | `DISPLAY [<scope>] [FIELDS <list>] [FOR <cond>] [OFF]` |
| DISPLAY STRUCTURE | `DISPLAY STRUCTURE` |
| DISPLAY MEMORY | `DISPLAY MEMORY` |
| ? | `? [<expr> [,<expr>...]]` |
| ?? | `?? <expr> [,<expr>...]` (no newline) |
| EJECT | `EJECT` (form feed / reset printer position) |
| REPORT FORM | `REPORT FORM <file> [<scope>] [FOR <cond>] [TO PRINT\|FILE <f>] [HEADING <text>] [PLAIN] [SUMMARY] [NOEJECT]` |
| LABEL FORM | `LABEL FORM <file> [<scope>] [FOR <cond>] [TO PRINT\|FILE <f>] [SAMPLE]` |

### Aggregate Commands
| Command | Syntax |
|---------|--------|
| COUNT | `COUNT [<scope>] [FOR <cond>] [TO <var>]` |
| SUM | `SUM [<scope>] <expr> [FOR <cond>] [TO <var>]` |
| AVERAGE | `AVERAGE [<scope>] <expr> [FOR <cond>] [TO <var>]` |

### Index Commands
| Command | Syntax |
|---------|--------|
| INDEX ON | `INDEX ON <expr> TO <file>` |
| SET INDEX TO | `SET INDEX TO [<file1>[,<file2>...]]` |
| REINDEX | `REINDEX` |
| SET ORDER TO | `SET ORDER TO [<n>]` (0=natural, 1+=index) |
| SET RELATION TO | `SET RELATION TO <expr> INTO <alias>` |

### Memory Variables
| Command | Syntax |
|---------|--------|
| STORE | `STORE <expr> TO <var>` |
| (assignment) | `<var> = <expr>` |
| RELEASE | `RELEASE <var>[,<var>...]` |
| RELEASE ALL | `RELEASE ALL [LIKE <pattern>\|EXCEPT <pattern>]` |
| PRIVATE | `PRIVATE <var>[,<var>...]` |
| PUBLIC | `PUBLIC <var>[,<var>...]` |
| SAVE TO | `SAVE TO <file> [ALL LIKE/EXCEPT <pattern>]` |
| RESTORE FROM | `RESTORE FROM <file> [ADDITIVE]` |

### User Input
| Command | Syntax |
|---------|--------|
| ACCEPT | `ACCEPT [<prompt>] TO <var>` |
| INPUT | `INPUT [<prompt>] TO <var>` |
| WAIT | `WAIT [<prompt>] [TO <var>]` |

### Screen I/O
| Command | Syntax |
|---------|--------|
| @ SAY | `@ <row>,<col> SAY <expr> [PICTURE <mask>]` |
| @ GET | `@ <row>,<col> GET <var> [PICTURE <mask>] [RANGE <lo>,<hi>]` |
| @ CLEAR TO | `@ <r1>,<c1> CLEAR TO <r2>,<c2>` |
| @ TO | `@ <r1>,<c1> TO <r2>,<c2> [DOUBLE]` |
| READ | `READ` |
| CLEAR | `CLEAR` |
| SET COLOR TO | `SET COLOR TO <spec>` |

### Programming
| Command | Syntax |
|---------|--------|
| DO <file> | `DO <file> [WITH <arg1>,<arg2>...]` |
| DO WHILE | `DO WHILE <cond>` |
| ENDDO | `ENDDO` |
| IF / ELSE / ENDIF | `IF <cond> ... [ELSE ...] ENDIF` |
| DO CASE / ENDCASE | `DO CASE / CASE <cond> ... [OTHERWISE ...] ENDCASE` |
| FOR / NEXT | `FOR <var> = <start> TO <end> [STEP <n>] ... NEXT` |
| EXIT | `EXIT` (break from loop) |
| LOOP | `LOOP` (continue loop) |
| PROCEDURE | `PROCEDURE <name>` |
| FUNCTION | `FUNCTION <name>` |
| PARAMETERS | `PARAMETERS <var1>[,<var2>...]` |
| RETURN | `RETURN [<expr>]` |
| SET PROCEDURE TO | `SET PROCEDURE TO [<file>]` |
| CANCEL | `CANCEL` |
| ON ERROR | `ON ERROR DO <procedure>` or `ON ERROR` (clear) |
| RETRY | `RETRY` (re-execute failed line, in error handler only) |
| SUSPEND | `SUSPEND` (pause program, return to dot prompt) |
| RESUME | `RESUME` (continue after SUSPEND) |
| QUIT | `QUIT` |

### Comments & Preprocessing
| Feature | Syntax |
|---------|--------|
| Full-line comment | `*` or `NOTE` at line start |
| Inline comment | `&& rest of line ignored` |
| Line continuation | `;` at end of line |
| Macro substitution | `&varname` or `&varname.` |

## Built-in Functions (57)

### Status (6)
`EOF()`, `BOF()`, `RECNO()`, `RECCOUNT()`, `DELETED()`, `FOUND()`

### String (17)
`SUBSTR(s,start[,len])`, `TRIM(s)`/`RTRIM(s)`, `LTRIM(s)`, `UPPER(s)`,
`LOWER(s)`, `AT(search,s)`, `LEN(s)`, `SPACE(n)`, `REPLICATE(s,n)`,
`LEFT(s,n)`, `RIGHT(s,n)`, `STR(n[,w[,d]])`, `VAL(s)`, `CHR(n)`,
`ASC(s)`, `TYPE(expr)`, `PADC(s,n)` or `PADL(s,n)` or `PADR(s,n)`

### Numeric (7)
`INT(n)`, `ROUND(n,d)`, `ABS(n)`, `MOD(a,b)`, `SQRT(n)`, `MAX(a,b)`, `MIN(a,b)`

### Date (9)
`DATE()`, `DTOC(d)`, `CTOD(s)`, `DAY(d)`, `MONTH(d)`, `YEAR(d)`,
`DOW(d)`, `CDOW(d)`, `CMONTH(d)`, `DTOS(d)`

### Screen (4)
`ROW()`, `COL()`, `PROW()`, `PCOL()`

### String Manipulation (3)
`STUFF(s,start,del,ins)`, `TRANSFORM(expr,pic)`, `ISALPHA(s)`,
`ISUPPER(s)`, `ISLOWER(s)`

### Misc (2)
`IIF(cond,true,false)`, `FILE(filename)`

### System (2)
`VERSION()`, `OS()`

### Keyboard (3)
`INKEY([secs])`, `LASTKEY()`, `READKEY()`

### Debug (4) — fully wired
`ERROR()`, `MESSAGE()`, `LINENO()`, `PROGRAM()`

## SET Options (14 active + 14 no-op)
| Option | Values | Default | Notes |
|--------|--------|---------|-------|
| TALK | ON/OFF | ON | |
| DELETED | ON/OFF | OFF | |
| EXACT | ON/OFF | OFF | |
| HEADING | ON/OFF | ON | |
| CONFIRM | ON/OFF | OFF | |
| BELL | ON/OFF | ON | |
| SAFETY | ON/OFF | ON | |
| CONSOLE | ON/OFF | ON | |
| DECIMALS | 0-18 | 2 | |
| DATE | AMERICAN/ANSI/BRITISH/... | AMERICAN | |
| FILTER | expression | (none) | |
| ORDER | 0-7 | 1 | |
| PROCEDURE | filename | (none) | |
| DEVICE | SCREEN/PRINT | SCREEN | Routes @SAY output |
| ECHO | ON/OFF | ON | No-op |
| MENU(S) | ON/OFF | ON | No-op |
| STATUS | ON/OFF | OFF | No-op |
| SCOREBOARD | ON/OFF | ON | No-op |
| ESCAPE | ON/OFF | ON | No-op |
| INTENSITY | ON/OFF | ON | No-op |
| UNIQUE | ON/OFF | OFF | No-op |
| FUNCTION | n TO str | — | No-op |
| PRINT | ON/OFF | OFF | No-op |
| HELP | ON/OFF | ON | No-op |
| CENTURY | ON/OFF | OFF | No-op |
| PATH | dir | — | No-op |

## Expression Operators

| Category | Operators |
|----------|-----------|
| Arithmetic | `+`, `-`, `*`, `/`, `**`/`^` (power), unary `-` |
| String | `+` (concat), `-` (trim-concat), `$` (substring test) |
| Comparison | `=`, `==` (exact), `<>`, `#`, `<`, `>`, `<=`, `>=` |
| Logical | `.AND.`, `.OR.`, `.NOT.`, `!` (.NOT. shorthand) |
| Logical literals | `.T.`, `.F.` |
| Field alias | `->` (e.g., `B->FNAME`) |
| Macro | `&varname` |

## Scope Clauses

`ALL`, `NEXT <n>`, `RECORD <n>`, `REST`, `FOR <condition>`

Used by: LIST, DISPLAY, DELETE, RECALL, REPLACE, COUNT, SUM, AVERAGE, COPY TO, SORT.

## Work Areas

10 independent work areas (1-10, aliased A-J or by USE...ALIAS name).
Each maintains: database file, record pointer, index files, filter, order.
Cross-area field access: `alias->fieldname`.

## Index System

B+ tree implementation with 4KB pages (NDX2 format). Leaf pages are
doubly-linked for efficient sequential traversal. Variable fanout adapts to
key length (e.g., 170 keys/page for 20-byte keys). Incremental index
maintenance: APPEND, REPLACE, PACK, ZAP, and APPEND FROM all update active
indexes automatically without requiring REINDEX. Backward-compatible reader
for old NDX1 flat-array format. Multiple indexes per work area via SET INDEX
TO or USE...INDEX. SET ORDER TO selects controlling index. REINDEX rebuilds
from current data.

## Screen I/O

Uses terminal service negotiation (term.h API). Falls back to line-mode
output when terminal service is unavailable. PICTURE masks support:
`9` (digit), `A` (alpha), `X` (any), `!` (uppercase), `#` (digit/space/sign).
Box drawing with single/double line characters. SET DEVICE TO PRINT
routes @SAY to sequential output with printer position tracking.
RANGE validation on @GET fields. EJECT resets printer position.

## Test Suite

52 tests covering: CREATE, navigation, expressions, functions, memory
variables, SET options, work areas, file operations, screen I/O,
programming constructs, procedures, functions, indexing, multi-index,
filters, scope clauses, comments, print device routing, exponentiation,
exact equality, string manipulation, SAVE/RESTORE, SET RELATION,
LIST OFF/TO, error handling (ON ERROR, SUSPEND/RESUME), report generation
(CREATE REPORT, REPORT FORM PLAIN/SUMMARY), label generation (CREATE LABEL,
LABEL FORM), B+ tree indexing (incremental insert/update, ZAP, PACK,
APPEND FROM with index), and more.
