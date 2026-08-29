# Dialect

Three COBOL dialects sit in this orbit. They are not interchangeable.

| | cobc370 | this compiler | majesty today |
|---|---|---|---|
| standard | X3.23-1974 | X3.23-1985 | GnuCOBOL's 85-shaped dialect |
| source | fixed format | fixed **and** free | free (`*> ` comments, lowercase) |
| character set | EBCDIC CP037 | ASCII | ASCII |
| packed hardware | yes | no (library) | host GnuCOBOL |
| I-O | QSAM, ISAM, VSAM | framing in [framing.md](framing.md) | line sequential + indexed + relative |
| SCREEN SECTION | no | yes (implementor) | yes |
| Report Writer | 1974 tables, complete | 85; v1 is majesty's subset | GnuCOBOL RW |
| `COMP` > 9 digits | refused | i64 | GnuCOBOL |
| `PERFORM … AFTER` | 74 reset order | 85 reset order | 85 |
| ODO receiving group | current count (74) | maximum (85) | 85 |
| ALTER | implemented (74) | gone from 85; refuse | unused |
| user-defined `FUNCTION-ID` | no | yes (2002; implementor here) | yes, load-bearing |
| `CALL … BY VALUE / RETURNING` | no | yes (C ABI) | yes |

Sharing a parser across the first two columns would be a defect
factory. The `AFTER` reset and the ODO receiving rule are enough;
cobc370 has the receipts in `COBOL74-ROADMAP.md`.

## COBOL 85 that majesty actually needs

From `~/majesty/src/cobol/` and `~/majesty/docs/cobol-programs.md`:

- Identification / Environment / Data / Procedure, including
  `CONFIGURATION SECTION`, `REPOSITORY`, `INPUT-OUTPUT SECTION`
- `COPY` of copybooks under `~/majesty/src/copy/`
- Data: levels 01–49, 77, 88; `PIC`; `VALUE`; `OCCURS` (fixed);
  `REDEFINES`; `COMP-3`; `COMP-5`; `signed-int`; `signed-short`;
  `POINTER`; `SYNC`
- Verbs: `MOVE`, arithmetic with `GIVING`, `IF`/`ELSE`/`END-IF`
  (also `IF … THEN`), `PERFORM`/`END-PERFORM` structured, `PERFORM
  WITH TEST AFTER`, and paragraph `PERFORM` (every gate program),
  `READ`/`WRITE`/`REWRITE`/`START`/`DELETE` with `AT END` / `NOT AT
  END` / `INVALID KEY` / `END-READ` / `END-WRITE`, `OPEN`/`CLOSE`,
  `DISPLAY`, `ACCEPT` of screens, `CALL`, `GOBACK`, `STOP RUN`, `EXIT`
- `EVALUATE … WHEN … WHEN OTHER … END-EVALUATE`, nested: `menu.cbl`
  is a gate and is built on it (ten programs use it corpus-wide)
- `STRING … DELIMITED BY SIZE/SPACE … WITH POINTER … END-STRING`:
  the report programs build every output filename with it, and
  taskdt builds the date string with it
- `INSPECT … TALLYING … FOR LEADING`, `INITIALIZE`, reference
  modification with arithmetic (`x(a + 1:length(x) - a)`): all in
  taskdt, on the menu gate path
- **User-defined functions** (`FUNCTION-ID`, `RETURNING`, `REPOSITORY.
  FUNCTION name`, `name(args)` and `name()`): see
  [functions.md](functions.md). gl030 and menu both stand on them.
- `CALL … USING BY VALUE … BY REFERENCE … RETURNING …` (clinkages.cbl)
- User-words containing `_` (`ltf_lineardate`, `is_valid`,
  `c_lineartofielded`) — not in 85; GnuCOBOL accepts them
- Report Writer: `RD`, `PAGE LIMIT`, heading/first/last detail,
  `TYPE PAGE HEADING` and `TYPE DETAIL`, `LINE` / `LINE PLUS`,
  `COLUMN`, `SOURCE`, `VALUE`, `INITIATE`/`GENERATE`/`TERMINATE`
- Intrinsic functions via `FUNCTION ALL INTRINSIC`; the gates use
  exactly `LOWER-CASE`, `UPPER-CASE`, `LENGTH`, `CURRENT-DATE`
- `CALL` of C by name using the SLOW-32 ABI. The C entry points are
  `du_lineartofielded`, `du_fieldedtolinear` and the rest of
  `~/majesty/src/c/dateutil.c`; `c_lineartofielded` is **not** C, it
  is a COBOL function wrapping the `CALL`. `CBL_GET_SCR_SIZE` is the
  runtime's.
- `ASSIGN` to a literal or to a data-name (`ws-output-filename`)
- `SHARING WITH ALL OTHER` — accept and ignore on this machine
  (single process, no file locking service yet); refuse only if a
  program depends on the lock semantics
- `OPTIONAL` files (`w001`)

## Implementor modules, named as such

Do not pretend these are in X3.23-1985:

- `ORGANIZATION IS LINE SEQUENTIAL`
- SCREEN SECTION and `DISPLAY`/`ACCEPT` of a screen-name
- `USAGE COMP-5`, `BINARY-CHAR [UNSIGNED]`, `SIGNED-INT`,
  `SIGNED-SHORT`, `UNSIGNED-SHORT`, `POINTER`
- User-defined functions (`FUNCTION-ID`) — COBOL 2002, not 1985
- `_` in user-words
- `ACCEPT … FROM ARGUMENT-VALUE / ARGUMENT-NUMBER` (nine programs
  take a `YYYYMM`/`YYYY` parameter that way; none of the v1 gates)
- `CBL_GET_SCR_SIZE` and other `CBL_*` runtime entry points as
  they appear
- GnuCOBOL `repository. function all intrinsic`

Each is documented, tested, and listed in diagnostics as an
implementor feature when we have to talk about it.

## COBOL 85 we will grow into, not v1

Nucleus Level 2 at full width (abbreviated conditions,
`CORRESPONDING`, `UNSTRING`, `INSPECT REPLACING`/`CONVERTING`, nested
programs, `REPLACE`). `STRING`, `INSPECT TALLYING`, `INITIALIZE` and
reference modification are **in v1**, at the width taskdt uses them —
see [plan.md](plan.md) Stage 9. Indexed alternate keys. Report Writer `CONTROL`/`SUM`/`USE BEFORE REPORTING`. These are
real 85; they are not on the majesty v1 path. CCVS-85 NC/SQ/IC is the
yardstick for that growth, not for v1. See [plan.md](plan.md).
