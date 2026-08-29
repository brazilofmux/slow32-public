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
- Verbs: `MOVE`, arithmetic with `GIVING`, `IF`/`END-IF`,
  `PERFORM`/`END-PERFORM` (structured; also paragraph form if it
  appears), `READ`/`WRITE`/`REWRITE`/`START`/`DELETE` with
  `AT END` / `INVALID KEY` / `END-READ` / `END-WRITE`, `OPEN`/`CLOSE`,
  `DISPLAY`, `ACCEPT` of screens, `CALL`, `GOBACK`, `STOP RUN`
- `EVALUATE` if a program uses it; otherwise refuse with a message
  until one does
- Report Writer: `RD`, `PAGE LIMIT`, heading/first/last detail,
  `TYPE PAGE HEADING` and `TYPE DETAIL`, `LINE` / `LINE PLUS`,
  `COLUMN`, `SOURCE`, `VALUE`, `INITIATE`/`GENERATE`/`TERMINATE`
- Intrinsic functions via `FUNCTION ALL INTRINSIC`
- `CALL` of C by name (`c_lineartofielded`, `c_fieldedtolinear`,
  `CBL_GET_SCR_SIZE`) using the SLOW-32 ABI
- `ASSIGN` to a literal or to a data-name (`ws-output-filename`)
- `SHARING WITH ALL OTHER` — accept and ignore on this machine
  (single process, no file locking service yet); refuse only if a
  program depends on the lock semantics
- `OPTIONAL` files (`w001`)

## Implementor modules, named as such

Do not pretend these are in X3.23-1985:

- `ORGANIZATION IS LINE SEQUENTIAL`
- SCREEN SECTION and `DISPLAY`/`ACCEPT` of a screen-name
- `USAGE COMP-5`, `BINARY-CHAR`, `SIGNED-INT`, `SIGNED-SHORT`,
  `POINTER`
- `CBL_GET_SCR_SIZE` and other `CBL_*` runtime entry points as
  they appear
- GnuCOBOL `repository. function all intrinsic`

Each is documented, tested, and listed in diagnostics as an
implementor feature when we have to talk about it.

## COBOL 85 we will grow into, not v1

Nucleus Level 2 completeness (abbreviated conditions, `CORRESPONDING`,
`STRING`/`UNSTRING`/`INSPECT` at full width, nested programs,
`INITIALIZE`, reference modification, `REPLACE`). Indexed alternate
keys. Report Writer `CONTROL`/`SUM`/`USE BEFORE REPORTING`. These are
real 85; they are not on the majesty v1 path. CCVS-85 NC/SQ/IC is the
yardstick for that growth, not for v1. See [plan.md](plan.md).
