# Stage 2: Navigation & Expressions

## Goal

Add record navigation (GO, SKIP, EOF/BOF) and a full expression
evaluator supporting string, numeric, date, and logical operations.
LIST/DISPLAY gain FOR clauses and field expressions.

## Prerequisites

- Stage 1 complete (DBF engine + minimal REPL)

## New Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| GO / GOTO | `GO TOP` / `GO BOTTOM` / `GO <n>` | Position to record |
| SKIP | `SKIP [<n>]` | Move forward/backward |
| LOCATE | `LOCATE FOR <condition>` | Find first matching record |
| CONTINUE | `CONTINUE` | Find next matching record |
| COUNT | `COUNT [FOR <cond>]` | Count records |
| SUM | `SUM <expr> [FOR <cond>]` | Sum numeric expression |
| AVERAGE | `AVERAGE <expr> [FOR <cond>]` | Average numeric expression |
| LIST | `LIST [<fields>] [FOR <cond>]` | List with filter |
| DISPLAY | `DISPLAY [<fields>]` | Show current record fields |
| ? / ?? | `? <expr> [, <expr> ...]` | Print expressions |

## New Functions

### Status Functions
`EOF()`, `BOF()`, `RECNO()`, `RECCOUNT()`, `DELETED()`, `FOUND()`

### String Functions
`SUBSTR(s,start,len)`, `TRIM(s)` / `RTRIM(s)`, `LTRIM(s)`,
`UPPER(s)`, `LOWER(s)`, `AT(search,target)`, `LEN(s)`,
`SPACE(n)`, `REPLICATE(s,n)`, `LEFT(s,n)`, `RIGHT(s,n)`,
`STR(n,len,dec)`, `VAL(s)`, `CHR(n)`, `ASC(s)`, `TYPE(expr)`

### Numeric Functions
`INT(n)`, `ROUND(n,dec)`, `ABS(n)`, `MOD(n,d)`, `SQRT(n)`,
`MAX(a,b)`, `MIN(a,b)`

### Date Functions
`DATE()`, `DTOC(d)`, `CTOD(s)`, `DAY(d)`, `MONTH(d)`, `YEAR(d)`,
`DOW(d)`, `CDOW(d)`, `CMONTH(d)`

### Logical Operators
`.AND.`, `.OR.`, `.NOT.`, comparison operators (`=`, `<>`, `<`, `>`, `<=`, `>=`),
string concatenation (`+`, `-` for trim-concatenate), `$` (substring test)

## Expression Evaluator

Recursive-descent parser producing values:

```c
typedef enum { VAL_CHAR, VAL_NUM, VAL_DATE, VAL_LOGIC, VAL_NIL } val_type_t;

typedef struct {
    val_type_t type;
    union {
        char str[256];
        double num;
        int32_t date;    // Days since epoch (Julian day number)
        int logic;       // 0 or 1
    };
} value_t;
```

Operator precedence (low to high):
1. `.OR.`
2. `.AND.`
3. `.NOT.`
4. `=`, `<>`, `<`, `>`, `<=`, `>=`, `$`
5. `+`, `-` (string concat / numeric add)
6. `*`, `/`
7. Unary `-`, `.NOT.`
8. `()`, function calls, field references, literals

## Source Files

```
src/
  expr.c          # Expression evaluator (parser + evaluator)
  expr.h
  func.c          # Built-in function implementations
  func.h
  date.c          # Date arithmetic (Julian day conversions)
  date.h
```

## Success Criteria

- `GO TOP`, `GO BOTTOM`, `GO n`, `SKIP` all work
- `? UPPER(TRIM(name))` evaluates nested expressions
- `LIST FOR age > 30 .AND. state = "CA"` filters correctly
- `LOCATE FOR name = "SMITH"` + `CONTINUE` finds records
- Date arithmetic works: `? DATE() - hire_date` gives number of days
- `COUNT FOR deleted()` counts soft-deleted records
