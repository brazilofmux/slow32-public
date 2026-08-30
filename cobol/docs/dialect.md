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
| `CALL … BY CONTENT` | yes | yes (Stage 31) | yes |

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
- **The command line** (GnuCOBOL's module, measured): `ARGUMENT-NUMBER`
  is the count without the program name; `ARGUMENT-VALUE` yields the
  arguments in turn from 1 and leaves the item unchanged past the end;
  `DISPLAY n UPON ARGUMENT-NUMBER` makes the next one n; `COMMAND-LINE`
  is the arguments joined by blanks. The emulator passes arguments
  after the program name (`slow32-fast prog.s32x 202608`).
- **`OCCURS m TO n DEPENDING ON d`**: laid out at n; d must be an
  integer item outside the table. A `MOVE` *of* a group the table sits
  in sends the group's current length -- the table at any depth below
  it (Stage 33): laid out at the maximum, the length is size - (max -
  d) x element; the table must be the last thing in the group (items
  following it would sit at variable locations, which this layout does
  not give them: refused by name). A *receiving* group with such a
  table takes its maximum length, the 85 rule (free/odomove; GnuCOBOL
  takes the current one).
- **`COPY`**: text-name as a word or literal; found as given, then
  `.cpy`, `.CPY`, `.cbl`, `.CBL`, beside the source and in the `-I`
  directories; `OF`/`IN library` accepted (the directories serve as the
  library); `SUPPRESS` accepted. **`REPLACING`** (Stage 26): each
  operand is `==pseudo-text==`, a literal, a word, or an identifier
  with its `IN`/`OF` qualifiers and subscripts; the copied text is
  rewritten token by token, leftmost pair first at each position,
  the replacement taking the copied token's line for diagnostics.
  Comment lines never take part; a debugging line (`D` in column 7)
  takes part in the matching as if the `D` were not there, and is
  dropped afterwards (there is no `WITH DEBUGGING MODE`) -- a `COPY`
  on a debugging line stays a comment. Nesting is limited to 8 deep;
  a program may expand any number of copybooks (SM101A has 116).
- **`CALL identifier`** (Stage 28): every unit registers its
  PROGRAM-ID and entry (and a CANCEL routine) from `.init_array`
  before `main`, so a name held in a data item is resolved at run
  time against every program linked into the executable, trailing
  spaces trimmed, case folded (the static link folds too; GnuCOBOL's
  dynamic lookup is case-sensitive -- docs/oracles.md). A name not in
  the executable stops the run unless the CALL has **`[ON]
  EXCEPTION|OVERFLOW` / `NOT [ON] EXCEPTION`**, which is the only
  exception there is; a literal CALL with the clause goes through the
  registry too, so the link does not demand the program. **`CANCEL`**
  puts every WORKING-STORAGE record of the program back to its initial
  state (an image kept in `.rodata`); open files are not closed by it.
- **Contained (nested) programs** (Stage 29): an `IDENTIFICATION
  DIVISION` after a program's last sentence begins a program it
  contains, closed by its own `END PROGRAM`; any depth up to 8. Each is
  a unit of its own -- entry, WORKING-STORAGE, files, paragraphs, its
  registry entry -- compiled from one shared symbol, file and paragraph
  table cut back on `END PROGRAM`. A contained program sees the
  containing programs' **`GLOBAL`** items (an 01 `IS GLOBAL` reaches its
  subordinates and 88s) and **`FD ... GLOBAL`** files with their records,
  by name lookup that falls outward once the program's own names miss;
  everything else is private, so two programs may declare the same
  names. **`USE GLOBAL`** procedures of the containing programs apply
  to a contained program's I/O: the compiler emits the choice after
  every I/O statement -- the program's own USE for the file, then for
  the open mode, then outward through the enclosing programs' GLOBAL
  ones (the runtime's result code says whether an unhandled error
  stops the run: 3 with no FILE STATUS, 2 with one). `PROGRAM-ID ...
  IS COMMON` is accepted (every program here may call every other, so
  it changes nothing); `IS INITIAL` re-initialises WORKING-STORAGE on
  every CALL (the CANCEL image). Not in a contained program: a REPORT
  or SCREEN SECTION. A contained program's link name is its own
  PROGRAM-ID, so two contained programs of one executable may not
  share a name.
- **`EXTERNAL`** (Stage 30): an 01/77 `IS EXTERNAL` is storage shared
  by name among every program of the executable -- the runtime hands
  out one block per name (`cob_external`, zeroed like GnuCOBOL's, grown
  if a later program declares it longer) and the record is addressed
  through a cell, as a LINKAGE item is; no VALUE. **`FD ... EXTERNAL`**
  is one file connector for every program naming the file: the first
  program to enter with it lends its image, its record area is a block
  shared under the file's name (so the records of every program's FD
  are the same bytes), and each program entering sets the connector's
  FILE STATUS to its own item and puts the previous one back on exit,
  so the program executing the statement is the one whose status is
  written (IC227A). A `SELECT` may leave `ASSIGN TO` empty only for an
  EXTERNAL file (the other program names it; GnuCOBOL always wants a
  name, and so does NIST IC227A-1 -- an X-card). A FILE STATUS item may now be in the LINKAGE SECTION:
  its address goes into the image at entry.
- **`CALL ... BY CONTENT`** (Stage 31): the callee is handed the
  address of a copy, taken from a runtime arena that behaves as a stack
  -- pushed before the CALL, released after it (`cob_content_push`/
  `_pop`), so recursion and nesting balance. An item, or a literal. A
  numeric literal handed to a CALL, by reference or by content, is its
  plain digits (a negative one zoned in the last digit), read through
  the callee's own picture (GnuCOBOL hands over a binary word instead
  -- docs/oracles.md; the 85 text leaves a literal's class to the
  callee). Not yet: BY CONTENT of a reference-modified item.
- **`UNSTRING`** (Stage 34): `UNSTRING src [DELIMITED BY [ALL] d [OR
  [ALL] d]...] INTO {r [DELIMITER IN r] [COUNT IN r]}... [WITH POINTER
  p] [TALLYING IN t] [[NOT] ON OVERFLOW] [END-UNSTRING]`. The runtime
  scans (`cob_unstr_*`): the characters up to the leftmost delimiter
  (the first listed wins at equal positions; ALL takes the repeats)
  go to the receiver by the MOVE rules -- an empty field gives spaces,
  or zero to a numeric receiver; with no DELIMITED BY each receiver
  takes as many characters as it holds (one fewer for a separate
  sign). Receivers left over when the source is exhausted are
  untouched; source left over when the receivers are is the overflow,
  as is a POINTER outside the source; TALLYING is incremented by the
  receivers acted on. NIST NC218A (125 tests) matches GnuCOBOL.
- **A whole group over an OCCURS DEPENDING ON table as an operand**
  (Stage 34) -- MOVE, STRING, UNSTRING, INSPECT, a comparison, DISPLAY
  -- has the group's *current* length: the operand becomes
  `(1:length)` with the length computed at run time (base + DEPENDING
  ON x element; `Ref.rm_odo`), so every path that handles reference
  modification handles it. Receivers are not operands and keep the
  maximum, the 85 rule. NC247A matches GnuCOBOL.
- **`INSPECT` as the text describes it** (Stage 35): one pass over the
  item, the phrases tried in order at each position; a phrase that
  matches takes the positions -- tallied or replaced -- and no later
  phrase sees them; with no match the position is passed over. `[BEFORE
  |AFTER] [INITIAL] x` on any phrase bounds its range, found in the
  item's original contents from its first character (AFTER absent: the
  phrase sees nothing; BEFORE absent: to the end). LEADING ends at the
  first position of its range the phrase does not take; FIRST takes
  once. A statement with both TALLYING and REPLACING is two
  statements, the tallying pass first. `ALL`/`LEADING`/`FIRST` take a
  list of operands (`FOR LEADING "S" AFTER x "T" AFTER y`).
  `CONVERTING from TO to [range]` is one single-character replacing
  phrase per character. A signed numeric DISPLAY item with the sign in
  a digit is inspected as though moved to an unsigned item, the sign
  put back afterwards. The compiler registers the phrases
  (`cob_inspect_begin/range/phrase/convert`), the runtime makes the
  pass (`cob_inspect_run`), the counts are added (`cob_inspect_count`).
  NIST NC115A, NC122A, NC216A (57 tests) and NC221A match GnuCOBOL.
- **`MOVE` / `ADD` / `SUBTRACT CORRESPONDING`** (Stage 36): the pairs are
  found at compile time (X3.23 6.4.2) -- items of the two groups with
  the same name and the same qualifiers below them, neither FILLER,
  neither with REDEFINES or OCCURS (a child with one is skipped with
  its subtree), no condition-names; two groups that correspond are
  searched further. MOVE moves a pair when at least one is elementary;
  ADD/SUBTRACT act on pairs of elementary numeric items, ROUNDED on
  each, ON SIZE ERROR once for the statement if any pair overflowed
  (that pair's receiver unchanged). The operands' own subscripts and
  qualification carry to every pair. Qualification may run 64 deep
  (NC207A's is 48). NC202A, NC207A, NC208A, NC222A, NC253A match.
- **PICTURE at 85 width** (Stage 37): `A`, `X` and `9` in any
  combination are an alphanumeric item (all `A` alphabetic; `9A9`,
  `AB9`, `XBA09` alphanumeric or alphanumeric-edited with `B`, `0`,
  `/`); `P` beside `Z`, `*` or a floating string is an edited picture
  with scaling positions (`ZZZPP`: the stored digits are the high ones,
  the editor gives P no character, DISPLAY shows the value with its low
  zeros); a `;` or `,` right after a picture is the separator, not a
  symbol (`PICTURE 99; VALUE 8`). A MOVE that scales a value up past 64
  bits (12345 into `9V9(17)`) truncates the high-order digits as the
  text says rather than overflowing (NC104A).
- **`DIVIDE ... REMAINDER` at full width** (Stage 38): the remainder is
  the dividend less the divisor times the quotient *as it would be
  stored before ROUNDED* -- the quotient truncated to the receiver's
  decimals (X3.23 6.9.4), recomputed for the purpose rather than read
  back -- so `ROUNDED` on the quotient is fine. With `ON SIZE ERROR`, a
  quotient that overflowed leaves the remainder alone, a remainder that
  overflows is the statement's size error too, and the clause runs for
  either. The remainder receiver may be numeric-edited. NC203A and
  NC251A match GnuCOBOL.
- **Level 66 `RENAMES`** (Stage 39): `66 name RENAMES a [THRU|THROUGH
  b]` after a record's entries names the storage from `a` to the end
  of `b` (or `a` alone) in that record. The names are the record's own:
  the 01 the entry follows (a REDEFINES 01 by its own name) is an
  implicit last qualifier, and `OF`/`IN` may be written. `a` alone and
  elementary makes an alias with `a`'s picture and usage; anything else
  a group. The 66 lives outside the record's tree (no subordinates,
  never a CORRESPONDING pair, no storage of its own), belongs to the
  record for `OF record`, and may carry 88s. Neither name may be a
  level 01/66/77/88 nor have OCCURS or lie in a table. NC252A (75
  tests) matches GnuCOBOL -- after a size-error fix it exposed: the
  overflow test and truncation of a numeric item with P positions are
  on its *stored* digits (`S99P` holds 2), so the descriptor of any
  item whose picture has P now carries the picture.
- **`REPLACE ==a== BY ==b== ...`** / **`REPLACE OFF`**: the same
  machinery over the source that follows the statement, until the
  next `REPLACE`; runs after every `COPY` has been expanded, so it
  sees copied text too. `== = ==` is a pseudo-text holding one `=`
  (`==` is a token of its own; a sign right after it still begins
  a numeric literal).
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
- **`LENGTH OF item`** (IBM register, in the corpus) is the same
  compile-time size as `FUNCTION LENGTH(item)`; a data item named
  `LENGTH` takes precedence.
- **`ACCEPT identifier`** (no `FROM`) reads one line from standard
  input, drops the newline, and moves it as alphanumeric text -- so
  into `9(3)v99` the line `12345` arrives as the decimal 12345 and
  truncates to `345.00`, as GnuCOBOL does. At end of file the item is
  left as it was.
- **Continuation lines** (`-` in column 7): inside a non-numeric
  literal the continuation's first non-blank must be the literal's
  quote and the previous line keeps its trailing blanks to column 72;
  outside one, the previous line's trailing blanks go and the join is
  at the continuation's first non-blank.
- **`WRITE ... ADVANCING PAGE`** (and a FORMFEED mnemonic) writes a
  form feed before (AFTER) or after (BEFORE) the record, GnuCOBOL's
  reading of a page on a line-sequential file.
- **`FD ... LINAGE IS n [LINES] [WITH FOOTING AT f] [LINES AT TOP t]
  [LINES AT BOTTOM b]`** (integers or integer items, taken at OPEN and
  at each new page) makes the file a print file with a logical page,
  laid out as GnuCOBOL lays it (fileio.c): a WRITE AFTER n LINES adds n
  to `LINAGE-COUNTER` and n-1 blank lines before the record, BEFORE n
  the same after it, no ADVANCING is BEFORE 1; past the last line the
  page is filled, the bottom and next top margins written, and the
  counter starts at 1; ADVANCING PAGE does the same at once; the
  record is written whole, trailing spaces kept. `LINAGE-COUNTER [OF
  file]` reads the counter (1 at OPEN). `WRITE ... [NOT] [AT]
  END-OF-PAGE` (or `EOP`) sees the footing reached or the page
  overflowed by that WRITE.
- **Report Writer, the page half at 85 width** (Stage 32): report
  group entries take their clauses in any order at any level (X3.23
  VIII-7): `LINE` begins a line of the group -- on the 01 too -- and
  `COLUMN`/`PICTURE`/`SOURCE`/`VALUE` make the entry a printable field
  of the current line, so an entry may be both (the elementary group
  `01 X TYPE DE LINE PLUS 1 COLUMN 1 PIC ... SOURCE ...`). `TYPE PAGE
  FOOTING`/`PF` groups are presented at each page end and at
  TERMINATE, on a page that was begun; RD `FOOTING n` bounds the body
  (LAST DETAIL defaults to it). `LINE-COUNTER` and `PAGE-COUNTER`
  `[OF report]` are items (four-byte, in the report block): INITIATE
  sets them 0 and 1, the first GENERATE begins page 1 without counting
  it, a page end counts; while a line's fields are moved LINE-COUNTER
  already holds that line's number, so a `SOURCE LINE-COUNTER` on the
  page heading prints 1. An RD without a PAGE clause is one endless
  page. A print file need not be LINE SEQUENTIAL: another organization
  takes each line as a space-filled record. Still not: `CONTROL`,
  `SUM`, `NEXT GROUP`, `GROUP INDICATE`, report heading/footing groups,
  `USE BEFORE REPORTING` (ISSUES-11).
- **`I-O-CONTROL`** (SAME AREA, RERUN, MULTIPLE FILE) is parsed and
  ignored. **`CLOSE ... REEL/UNIT`** does nothing: a disk file has one
  reel. **`RESERVE`, `PADDING CHARACTER`, `RECORD DELIMITER`** are
  accepted and ignored.
- **`SPECIAL-NAMES`**: `SWITCH-1`..`SWITCH-8` with ON/OFF STATUS
  condition-names and `SET mnemonic TO ON/OFF` (all off at start);
  `ALPHABET name IS STANDARD-1|NATIVE` is the native (ASCII) sequence,
  any other alphabet is recorded and refused where it would be used;
  `SYSIN|SYSOUT|CONSOLE|SYSERR|FORMFEED IS mnemonic` for ACCEPT FROM,
  DISPLAY UPON and ADVANCING. **`DECIMAL-POINT IS COMMA`** (Stage
  27): a comma tight between digits is the decimal point of a numeric
  literal (`12,5`; `1,5` outside this mode is still the old error),
  and `.` and `,` trade places in every PICTURE -- `ZZ.ZZZ,99` edits
  to `12.345,67`. The clause may arrive by COPY, so it is applied
  once the text is whole (`apply_decimal_point`): literals joined,
  pictures rewritten into the ordinary form; the runtime
  (`cob_dp_comma`, set on entry and restored on exit like the
  collating sequence) swaps the two characters on the way out of
  editing, into de-editing, and in DISPLAY of a scaled numeric item
  or literal. `CURRENCY SIGN` and `SYMBOLIC CHARACTERS` are refused
  by name.
- **`SIGN IS LEADING|TRAILING [SEPARATE]`** on a signed numeric
  DISPLAY item or on a group (reaching its subordinate ones): a
  leading overpunch is on the first digit; SEPARATE adds a character.
- **`DECLARATIVES`**: `USE [GLOBAL] AFTER [STANDARD] ERROR|EXCEPTION
  PROCEDURE ON {file | INPUT | OUTPUT | I-O | EXTEND}`. After an I/O
  statement whose condition its own AT END / INVALID KEY clause does
  not handle, the applicable section (the file's, else the open
  mode's) is performed and execution continues with the next statement.
  A file with no FILE STATUS and no USE still stops the run on an
  error.
- **`PROGRAM COLLATING SEQUENCE IS alphabet`** (OBJECT-COMPUTER) with an
  `ALPHABET` of literal phrases: the characters named take the first
  collating positions in that order (`ALSO` shares one, `THROUGH` runs
  a range), the rest follow in native order; alphanumeric comparisons
  (and so SORT keys) use it, and `LOW-VALUE`/`HIGH-VALUE` are its first
  and last characters. Set at the unit's entry, the caller's restored
  at its exit.
- **`I-O-CONTROL SAME RECORD AREA FOR f1 f2 ...`**: the files share one
  record area (the later files' 01s redefine the first's).
- **`MERGE`** is a stable SORT of its USING files (they are sorted,
  so that is the merge). **Qualified procedure-names** (`para OF
  section`) and the same paragraph name in different sections are
  taken; an unqualified reference means the current section's.
- **A level 77 item may REDEFINES** another (not OCCURS). **`88 ...
  VALUE ALL literal`** is taken. **`[AT] END`, `INVALID [KEY]`,
  `[ORGANIZATION IS] SEQUENTIAL`, `RELATIVE [KEY IS]`** -- the optional
  words are optional.
- **Relative files** are fixed slots of `4 + maximum record` bytes
  framed with the mode-V RDW, zero for an empty slot (docs/indexed.md);
  records may be shorter than the slot (`RECORD CONTAINS m TO n`).
- **SORT** holds its records in memory and is stable; `WITH DUPLICATES
  IN ORDER` is accepted and is what happens anyway; `COLLATING
  SEQUENCE` and `MERGE` are refused.
- **Calendar functions** (`INTEGER-OF-DATE`, `DATE-OF-INTEGER`,
  `DAY-OF-INTEGER`, `INTEGER-OF-DAY`): integer 1 is 1601-01-01; an
  invalid argument gives 0; a value DISPLAYed directly shows ten,
  eight and seven digits respectively -- GnuCOBOL's widths, measured.
- **`SPECIAL-NAMES` `CLASS name IS lit [THROUGH lit] ...`** builds a
  256-entry membership table over the native collating sequence; other
  `SPECIAL-NAMES` clauses are refused by name.

## COBOL 85 we will grow into, not v1

Nucleus Level 2 at full width (abbreviated conditions; `UNSTRING`
landed in Stage 34, `INSPECT` at full width in Stage 35,
`CORRESPONDING` in Stage 36,
`REPLACE` and `COPY REPLACING` landed in Stage 26, contained programs
with `GLOBAL` in Stage 29). `STRING`, `INSPECT TALLYING`, `INITIALIZE` and
reference modification are **in v1**, at the width taskdt uses them —
see [plan.md](plan.md) Stage 9. Indexed alternate keys. Report Writer `CONTROL`/`SUM`/`USE BEFORE REPORTING`. These are
real 85; they are not on the majesty v1 path. CCVS-85 NC/SQ/IC is the
yardstick for that growth, not for v1. See [plan.md](plan.md).
