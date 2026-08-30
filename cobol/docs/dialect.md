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
| user-defined `FUNCTION-ID` (2002) | no | **no — corpus rewritten to `CALL`** | yes, today |
| `CALL … BY VALUE / RETURNING` | no | yes, C-ABI implementor only | yes |

Sharing a parser across the first two columns would be a defect
factory. The `AFTER` reset and the ODO receiving rule are enough;
cobc370 has the receipts in `COBOL74-ROADMAP.md`.

## COBOL 85 that majesty actually needs

From `~/majesty/src/cobol/` and `~/majesty/docs/cobol-programs.md`:

- Identification / Environment / Data / Procedure, including
  `CONFIGURATION SECTION` and `INPUT-OUTPUT SECTION` (`REPOSITORY`
  leaves with the rewrite)
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
- **Not** user-defined functions. The corpus uses them today
  (`c_lineartofielded(x)`, `taskdt()`), and is being rewritten to
  plain `CALL … USING` — see [functions.md](functions.md). The
  compiler never learns `FUNCTION-ID` or `REPOSITORY`.
- `CALL … USING BY VALUE … BY REFERENCE … RETURNING …` — only in
  `clinkages.cbl`, only to reach C; C-ABI implementor module
- User-words containing `_` (`ltf_lineardate`, `is_valid`) — not in
  85; whether the rewrite also renames them is an open question
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
- `CALL … BY VALUE` / `CALL … RETURNING` — the seam to C, confined
  to `clinkages.cbl`
- `_` in user-words (pending the rewrite decision)
- `ACCEPT … FROM ARGUMENT-VALUE / ARGUMENT-NUMBER` (nine programs
  take a `YYYYMM`/`YYYY` parameter that way; none of the v1 gates)
- `CBL_GET_SCR_SIZE` and other `CBL_*` runtime entry points as
  they appear
- GnuCOBOL `repository. function all intrinsic`

Each is documented, tested, and listed in diagnostics as an
implementor feature when we have to talk about it.

## Implementor conventions, as built (Stage 1)

Where the 1985 text says "implementor-defined", this is what
`s32-cobc` does. Each is GnuCOBOL's behaviour unless a reason is
given, because majesty's `.prn` oracles were produced under it.

- **Uninitialised WORKING-STORAGE**: alphanumeric to spaces, numeric
  to zero (in the item's usage). The standard leaves it undefined;
  majesty was written against GnuCOBOL's rule. Note that GnuCOBOL's
  own rule is per dialect: `default.conf` says `defaultbyte: init`,
  `cobol85.conf` says `none` (a numeric DISPLAY item under OCCURS came
  out as spaces there). Tests compiled with `-std=cobol85` must not
  read an item before setting it.
- **MOVE of a non-integer numeric item to an alphanumeric item** is
  refused, as the standard and GnuCOBOL both do ("invalid MOVE").
- **LINE SEQUENTIAL** (implementor module; measured against GnuCOBOL
  4.0-early-dev): a record is payload then `\n`; trailing spaces are
  removed on WRITE (an all-space record is an empty line); on READ the
  record area is space-filled beyond the line and a `\r` before the
  `\n` is dropped; a missing input file is status 35 (05 and at-end on
  the first READ when `OPTIONAL`); at end is 10. **One divergence,
  chosen:** a line longer than the record area is truncated with
  status 04 and the rest of the line discarded, per framing.md.
  GnuCOBOL 4 instead delivers the remainder as further records with
  status 06. No majesty file has such a line; a program that depends
  on the split would be wrong on both.
- **`CALL`**: literal program-names only; `BY REFERENCE` passes the
  address, `BY VALUE` an integer item of up to four bytes widened to a
  word (or an integer literal), `RETURNING` takes `r1` into an integer
  item; at most eight arguments (the C ABI's registers). The
  program-name literal is the linker symbol, lower-cased, with
  anything but letters, digits and `_` turned into `_` -- so `CALL
  'du_lineartofielded'` reaches C directly and `CALL 'c_lineartofielded'`
  reaches the COBOL unit of that `PROGRAM-ID`. `CANCEL` is accepted
  and does nothing. `EXIT PROGRAM` returns like `GOBACK`.
- **`STRING` without `DELIMITED BY`**: the 1985 text requires the
  phrase on every source; GnuCOBOL lets it be omitted and takes
  `SIZE`, and `taskdt.cbl` writes it that way. Accepted, as `SIZE`.
- **Intrinsic functions** (1989 amendment): `UPPER-CASE`,
  `LOWER-CASE`, `LENGTH` (folded at compile time; a variable-length
  reference modification is refused), `CURRENT-DATE` (the guest clock
  through the emulator, local time as the guest libc gives it,
  `YYYYMMDDhhmmsshh+hhmm`).
- **`GOBACK`** is accepted (IBM's word, not in X3.23-1985 -- GnuCOBOL
  `-std=cobol85` refuses it; majesty uses it everywhere). In the main
  program it is `STOP RUN`.
- **ADVANCING PAGE** is refused: majesty's `.prn` files carry no form
  feed, and a line-sequential print file has nowhere to put one.
- **`COMP`/`BINARY` width**: 2, 4, 8 bytes for 1-4, 5-9, 10-18 digits.
  IBM's table and the natural fit for the SLOW-32 C types. GnuCOBOL's
  default is 1-2-4-8; the difference is one- and two-digit items, and
  it only shows in a layout (REDEFINES, SYNC groups handed to C).
- **`COMP-5` width**: 1, 2, 4, 8 bytes for 1-2, 3-4, 5-9, 10-18
  digits -- GnuCOBOL's, because COMP-5 is GnuCOBOL's usage and
  majesty's `pic 9 comp-5` is one byte there. DISPLAY of a COMP-5 or
  C-ABI item shows the field's full capacity (3, 5, 10, 19 digits),
  not the picture's digits; DISPLAY of a COMP item shows the picture's.
- **Numeric DISPLAY sign**: trailing overpunch; a negative last digit
  is `p`..`y` (X'70'..X'79'). `SIGN SEPARATE` is not implemented yet.
- **DISPLAY of a numeric item**: a leading `+`/`-` when the picture is
  signed, every digit of the picture, and a `.` inserted where `V`
  falls (`pic 9(3)v99 value 1.5` displays `001.50`).
- **`HIGH-VALUE`** is X'FF', **`LOW-VALUE`** X'00'; the collating
  sequence is ASCII.
- **User-words** may contain `_`.
- **Floating comment** `*>` is accepted in both formats.

## COBOL 85 we will grow into, not v1

Nucleus Level 2 at full width (abbreviated conditions,
`CORRESPONDING`, `UNSTRING`, `INSPECT REPLACING`/`CONVERTING`, nested
programs, `REPLACE`). `STRING`, `INSPECT TALLYING`, `INITIALIZE` and
reference modification are **in v1**, at the width taskdt uses them —
see [plan.md](plan.md) Stage 9. Indexed alternate keys. Report Writer `CONTROL`/`SUM`/`USE BEFORE REPORTING`. These are
real 85; they are not on the majesty v1 path. CCVS-85 NC/SQ/IC is the
yardstick for that growth, not for v1. See [plan.md](plan.md).
