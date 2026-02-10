# dBase III Clone — Implementation Status

Current as of February 2026. 44/44 tests passing.

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
src/index.c      — NDX index files (sorted array, binary search)
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
| USE | `USE [<file>] [INDEX <ndx1>[,<ndx2>...]] [ALIAS <name>]` |
| CLOSE | `CLOSE` |
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
| LIST | `LIST [<scope>] [FIELDS <list>] [FOR <cond>] [OFF]` |
| DISPLAY | `DISPLAY [<scope>] [FIELDS <list>] [FOR <cond>] [OFF]` |
| DISPLAY STRUCTURE | `DISPLAY STRUCTURE` |
| DISPLAY MEMORY | `DISPLAY MEMORY` |
| ? | `? [<expr> [,<expr>...]]` |
| ?? | `?? <expr> [,<expr>...]` |

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

### Memory Variables
| Command | Syntax |
|---------|--------|
| STORE | `STORE <expr> TO <var>` |
| (assignment) | `<var> = <expr>` |
| RELEASE | `RELEASE <var>[,<var>...]` |
| RELEASE ALL | `RELEASE ALL` |
| PRIVATE | `PRIVATE <var>[,<var>...]` |
| PUBLIC | `PUBLIC <var>[,<var>...]` |

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
| @ GET | `@ <row>,<col> GET <var> [PICTURE <mask>]` |
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
| QUIT | `QUIT` |

### Comments & Preprocessing
| Feature | Syntax |
|---------|--------|
| Full-line comment | `*` or `NOTE` at line start |
| Inline comment | `&& rest of line ignored` |
| Line continuation | `;` at end of line |
| Macro substitution | `&varname` or `&varname.` |

## Built-in Functions (39)

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
`DOW(d)`, `CDOW(d)`, `CMONTH(d)`

## SET Options (13)
| Option | Values | Default |
|--------|--------|---------|
| TALK | ON/OFF | ON |
| DELETED | ON/OFF | OFF |
| EXACT | ON/OFF | OFF |
| HEADING | ON/OFF | ON |
| CONFIRM | ON/OFF | OFF |
| BELL | ON/OFF | ON |
| SAFETY | ON/OFF | ON |
| CONSOLE | ON/OFF | ON |
| DECIMALS | 0-18 | 2 |
| DATE | AMERICAN/ANSI/BRITISH/FRENCH/GERMAN/ITALIAN/JAPAN | AMERICAN |
| FILTER | expression or empty | (none) |
| ORDER | 0-7 | 1 |
| PROCEDURE | filename or empty | (none) |

## Expression Operators

| Category | Operators |
|----------|-----------|
| Arithmetic | `+`, `-`, `*`, `/`, unary `-` |
| String | `+` (concat), `$` (substring test) |
| Comparison | `=`, `<>`, `#`, `<`, `>`, `<=`, `>=` |
| Logical | `.AND.`, `.OR.`, `.NOT.` |
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

Flat sorted-array implementation (not B-tree). Binary search for SEEK/FIND.
Multiple indexes per work area via SET INDEX TO or USE...INDEX.
SET ORDER TO selects controlling index. REINDEX rebuilds from current data.

## Screen I/O

Uses terminal service negotiation (term.h API). Falls back to line-mode
output when terminal service is unavailable. PICTURE masks support:
`9` (digit), `A` (alpha), `X` (any), `!` (uppercase), `#` (digit/space/sign).
Box drawing with single/double line characters.

## Test Suite

44 tests covering: CREATE, navigation, expressions, functions, memory
variables, SET options, work areas, file operations, screen I/O,
programming constructs, procedures, functions, indexing, multi-index,
filters, scope clauses, comments, and more.
