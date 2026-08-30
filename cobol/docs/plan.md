# Plan

No code until this document says a stage has started. The stages
are ordered so each one is a claim the project can make, and so
work that shares machinery lands together. Sizes are honest
guesses in cobc370's units: **S** an afternoon, **M** a day, **L**
several.

## Stage 0 — specifications (this directory)

Done when the docs here match the rulings and someone can implement
Stage 1 without inventing a product. **Done 2026-08-29.**

## Prerequisite -- the corpus rewrite, in `~/majesty` **DONE**

Not a stage of this compiler. Landed as majesty `1da955d`
(2026-08-29): the seven C-bridge functions, `taskdt`, and every
caller are `PROGRAM-ID` / `CALL ... USING`; `REPOSITORY` and
`IF ... THEN` are gone from them. Verified under GnuCOBOL with
`batch.sh` unchanged and all 12 `reports_cobol/*.prn` byte-identical
to a same-day baseline, so the oracle `.prn` files are now produced
from 1985 source. Stage 6 is open. Details and the receiver-size
hazard the pass turned up: [functions.md](functions.md). The
pure-COBOL date-function family is still 2002 and is listed under
After v1.

## Stage 1 — host skeleton + PICTURE + hello **L** — DONE 2026-08-29

- Directory layout as in [architecture.md](architecture.md)
- Fixed-format and free-format readers
- Recursive-descent parser for Identification Division,
  `WORKING-STORAGE` elementary items, `DISPLAY` of literals,
  `STOP RUN` / `GOBACK`; `*>` comments; lowercase everything
- Ragel `picture.rl` re-hosted; software edit descriptor, not `ED`
- Emit SLOW-32 assembler, link, run on the emulator
- Refuse unimplemented with a message
- Tests: hello, a refused undeclared name, a refused 74-only verb
  (`ALTER`)

Done: a `.cbl` becomes a `.s32x` that prints a line.

What landed: `src/s32-cobc.c` (~1,100 lines), `picture.rl` re-hosted
with `picture.c` rewritten around a software edit descriptor,
`libcob/libcob.c` (init, stop, DISPLAY of every usage majesty
declares), `tests/run-tests.sh` with three gates -- PICTURE synthesis
against a hand-checked table, programs against `.expected` **and**
against host GnuCOBOL when present, refusals against their message.
Beyond the list above, because VALUE made them free: DISPLAY of
identifiers (all usages), figurative constants, `WITH NO ADVANCING`,
`X'..'` literals, `END PROGRAM`, `IS INITIAL`, sections and
paragraphs as labels, `CONTINUE`, `EXIT`. The DBT runs the output.
Rulings made here: command name `s32-cobc`; format by flag, fixed
default; `_` accepted in user-words; COMP/COMP-5 sizes and DISPLAY
conventions in [dialect.md](dialect.md).

## Stage 2 — data division + MOVE + COMP integer **L** — DONE 2026-08-29

- Groups, `REDEFINES`, `OCCURS` (fixed, three levels is enough;
  85 allows seven, grow when a program asks), subscripts that are
  data-names (`name-of-day(dow-plus-1)`)
- Level 77, level 88 condition-names and `SET`-free tests of them
  (`if not is_valid`) — every gate program uses 88s
- Qualification `OF`/`IN` — `com-id in company-record` in gl030,
  where the same name lives in two FDs
- User-words containing `_` (`ltf_lineardate`, `is_valid`): GnuCOBOL
  extension, required by the corpus
- Conversion matrix, alphanumeric and COMP integer hot cases
- `IF` / `ELSE` / `END-IF`, including `IF … THEN` (taskdt)
- Structured `PERFORM … UNTIL`, `PERFORM … WITH TEST AFTER`, and
  **paragraph `PERFORM`** — it is not "if it appears"; it appears in
  every gate program
- `SECTION`s in the Procedure Division (`main-logic section.`),
  `EXIT`, `END PROGRAM name.`
- `OPEN`/`CLOSE` of several files in one statement; `MOVE` to several
  receivers; figurative constants

Done: `MOVE` and `ADD` of `PIC S9(8) COMP` and `PIC X(n)` match
GnuCOBOL on checked-in tests.

What landed: the Data Division as a tree (`Sym[]` with parent/child,
layout with `REDEFINES` overlay and `SYNC` alignment, images built
per record with `VALUE` replicated across `OCCURS`), subscripts
literal / data-name / `name +- n`, qualification, 88s with `THRU` and
`SET ... TO TRUE`, a runtime descriptor shared by compiler and
`libcob` (`libcob/cobrt.h`), `cob_move` over the unedited cells of the
matrix, `cob_cmp`/`cob_class`, a scaled-i64 numeric stack for the four
arithmetic verbs (`GIVING`, several receivers, scale alignment, digit
truncation), IF with `NEXT SENTENCE`, every `PERFORM` form (paragraph,
`THRU`, `TIMES`, `UNTIL`, `VARYING ... AFTER`, `WITH TEST AFTER`,
inline) over a runtime PERFORM stack with an exit check at every
paragraph and section end, `GO TO ... DEPENDING ON`, `SET UP/DOWN BY`.
Hot cases inline: COMP-integer MOVE/ADD/SUBTRACT/compare, equal-size
alphanumeric MOVE as `memcpy`. Tests: `tables`, `move`, `arith`,
`control`, all oracle-agreed; five new refusals. Not in: `ROUNDED`,
`ON SIZE ERROR`, `REMAINDER`, edited receivers (stage 3), reference
modification (stage 9), `OCCURS DEPENDING ON`, `SIGN SEPARATE`,
`MOVE CORRESPONDING`, arithmetic expressions inside conditions.

## Stage 3 — decimal library + edited MOVE **L** — DONE 2026-08-29

- Canonical numeric, `COMP-3`, DISPLAY numeric, scale, `ROUNDED`,
  `ON SIZE ERROR`
- Edited pictures, de-edit; `----,---,--9.99` with **negative**
  values (see [report-writer.md](report-writer.md))
- `COMPUTE`, `ADD … GIVING`

Done: cobc370's `arith`/`compute`/`edtest`-shaped tests, rewritten
as 85, green against GnuCOBOL. Majesty amounts (`pic s9(9)v99
comp-3`) round-trip.

What landed: `libcob/cobedit.h`, the software edit descriptor applied
and reversed -- zero suppression, `*` fill, floating `+ - $` with
insertion characters inside the string, fixed signs, `CR`/`DB`, the
point ending suppression, `BLANK WHEN ZERO`, the all-suppressed zero;
de-editing on any numeric-edited sender (`MOVE ed TO num`), so the 85
feature IBM ANS COBOL lacked is in. `tests/fixed/edit.cbl` puts every
picture majesty prints through -1234567.89, -5, **-0.05**, 0, a
9-digit value and 12.34, and GnuCOBOL agrees on all of it -- the
small negative under `----,---,--9.99` that cobc370 misplaced lands
against the digits here. `COMPUTE` with a recursive-descent expression
grammar (`+ - * / **`, unary minus, parentheses) emitted onto the
numeric stack; the same expressions as condition operands (a
parenthesis is classified as condition or expression by lookahead);
`ROUNDED` per receiver (nearest, ties away from zero); `ON SIZE ERROR`
/ `NOT ON SIZE ERROR` on every arithmetic verb, with the receiver
unchanged on overflow and on division by zero; `REMAINDER`;
numeric-edited `GIVING`/`COMPUTE` receivers. Division carries the
operands' larger scale plus six guard digits, and GnuCOBOL agreed on
`1 / 3 * 3` (0.99) and on `tot / 3 ROUNDED` at eleven digits.
Refused as the text says: a numeric literal `VALUE` on a numeric-edited
item (GnuCOBOL `-std=cobol85` refuses it too), `ROUNDED MODE` (2002).
Not in: `SIGN SEPARATE`, `**` with a fractional or negative exponent,
`REMAINDER` beside `ROUNDED` or `SIZE ERROR`, multiplication past 18
digits (i64 is the canonical numeric; majesty's amounts are 11).

## Stage 4 — line sequential I-O **M** — DONE 2026-08-29

- `SELECT` / `FD` / `OPEN` / `READ` / `WRITE` / `CLOSE`
- `ORGANIZATION IS LINE SEQUENTIAL`; `BLOCK CONTAINS` accepted and
  ignored; `SHARING WITH ALL OTHER` accepted and ignored
- `ASSIGN` to literal and to data-name
- `AT END` / `NOT AT END` / `END-READ` / `OPTIONAL`
- `STRING … DELIMITED BY SIZE … INTO` and `FUNCTION LOWER-CASE`:
  gl022/gl023/gl030 build each per-company output filename with
  exactly that pair before `OPEN OUTPUT`

Done: read `data/descriptions_fixed_width.txt` (from majesty, in
place or a stripped fixture), write a copy, diff.

What landed: `SELECT` with every clause the corpus writes (`OPTIONAL`,
`ASSIGN` to literal or data-name, `ORGANIZATION`, `ACCESS`, `RECORD
KEY`, `FILE STATUS`, `SHARING` accepted and ignored), `FD` with
`BLOCK`/`RECORD CONTAINS`, `LABEL`, `DATA RECORD`, `REPORT IS`,
`RECORDING MODE F`; every 01 under an FD a view of one record area;
a `cob_file` block per file in `.data` (`libcob/cobrt.h`); `OPEN`
(all four modes, several files), `CLOSE`, `READ [INTO] AT END / NOT
AT END`, `WRITE [FROM] [BEFORE/AFTER ADVANCING n LINES]`; file status
00/04/05/10/30/35/41/42/47/48, a hard error with no FILE STATUS
stopping the run as GnuCOBOL does. Line sequential: payload then
`\n`, trailing spaces removed on WRITE, space-filled on READ, `\r`
dropped, an over-long line truncated with status 04 (GnuCOBOL 4
splits it into further records with 06 -- see dialect.md). Fixed
sequential: `LRECL` bytes, nothing else. `STRING` in full (`DELIMITED
BY SIZE`/literal/item, `WITH POINTER`, `ON OVERFLOW`), `FUNCTION
UPPER-CASE`/`LOWER-CASE` as operands anywhere. Programs now link the
MMIO libc (`--mmio 64K`); the harness runs each program, and the
oracle, in a fresh copy of `tests/data/`. The done-criterion was run
against majesty's real file in place: 3,113 records copied
byte-identical, under `slow32-fast` and under `slow32-dbt`.

## Stage 5 — indexed I-O **L** — DONE 2026-08-29

- Default path in [indexed.md](indexed.md)
- `gl039` compiles and runs
- Random `READ … KEY IS` by `desc-id`, `INVALID KEY`, `END-READ`

Done: gl039 then a tiny reader prints descriptions by id.

What landed: the default path in [indexed.md](indexed.md) -- fixed
slots plus our own key file, the table sorted in memory; `OPEN`
INPUT/OUTPUT/I-O, `WRITE`, `READ` by key (`KEY IS` or ACCESS RANDOM),
`READ NEXT`, `REWRITE` (indexed and fixed sequential), `DELETE`,
`START` with every relation, `INVALID KEY` / `NOT INVALID KEY` on all
of them; the `RECORD KEY` checked to be an item of the record. **The
real gl039.cbl compiles unchanged** and, run in place against
majesty's data, writes all 3,113 descriptions in 0.13 s; a five-line
reader then fetches four of them by id under `slow32-dbt`, matching
`grep` on the source file, and reports 23 for the fifth. One
documented divergence from GnuCOBOL ([oracles.md](oracles.md)):
`REWRITE` of an absent key under dynamic access is 23 by the text,
21 by GnuCOBOL. Not in: `ALTERNATE RECORD KEY`, RELATIVE files
(after v1), slot reuse after `DELETE`, a `SORT`-shaped rebuild.

## Stage 6 — subprograms and the C bridge **M** — DONE 2026-08-29

[functions.md](functions.md). Plain 1985 Inter-Program Communication,
plus the C seam.

- `CALL 'name' USING …` to another COBOL `PROGRAM-ID`; `LINKAGE
  SECTION`; `PROCEDURE DIVISION USING …`; `GOBACK`
- Several units per source file (`clinkages.cbl` holds four),
  each closed by `END PROGRAM`
- `CALL 'du_…' USING BY VALUE … BY REFERENCE … RETURNING …` — the
  C-ABI implementor clauses, confined to `clinkages.cbl`
- `dateutil.c` compiled for SLOW-32 and linked

Done: a test program `CALL`s `c_lineartofielded` through the
rewritten `clinkages.cbl` and prints the fielded date;
`c_isvaliddate` rejects a bad one.

What landed: several program units per source file, each closed by
`END PROGRAM`, each a linker-visible function named after its
`PROGRAM-ID` on the SLOW-32 C ABI (`docs/lowering.md`'s ruling: one
convention, so COBOL, C and Fortran link with no glue); the first
unit of an executable also gets the `main` wrapper, `-m` makes a
module of subprograms only. `LINKAGE SECTION`; `PROCEDURE DIVISION
USING` fills a cell per LINKAGE record from the argument registers
and every reference to a LINKAGE item goes through that cell; `CALL
'literal' USING` with `BY REFERENCE` (addresses), `BY VALUE`
(integer items up to a word, widened; literals), `RETURNING` into an
integer item from `r1`; `GOBACK` and `EXIT PROGRAM` return; `CANCEL`
is a no-op (everything is linked statically). `compile.sh` takes
several `.cbl`, `.c` and `.s32o` files; the harness takes a `.link`
file beside a test. The done-criterion ran for real: a program
`CALL`ing `c_lineartofielded`, `c_isvaliddate` and
`c_fieldedtolinear` through majesty's rewritten `clinkages.cbl` over
majesty's `dateutil.c`, compiled by the SLOW-32 C toolchain, printed
under `slow32-dbt` exactly what GnuCOBOL printed from the same three
sources. Not in: dynamic `CALL identifier`, `BY CONTENT`, more than
eight arguments (the stack case), `ON EXCEPTION`, `IS INITIAL`
re-initialisation, nested (contained) programs, `GLOBAL`/`EXTERNAL`.

## Stage 7 — Report Writer, cheap half **L** — DONE 2026-08-29

- [report-writer.md](report-writer.md) v1 subset
- Print files are line sequential

Done: **gl022, gl023, gl030** match `reports_cobol/`. This is the
first product claim. GnuCOBOL can still be on the rest of the
majesty path.

**Achieved.** `gl022.cbl`, `gl023.cbl` and `gl030.cbl` compile
unchanged (gl030 with the rewritten `clinkages.cbl` and majesty's
`dateutil.c`), run in place against majesty's data on `slow32-fast`
(0.03 s, 0.02 s, 0.08 s) and under `slow32-dbt`, and all six reports
-- `chartofaccounts1-*.prn`, `chartofaccounts2-*.prn`,
`journal-*.prn`, 1,647 lines -- are byte-identical to
`~/majesty/reports_cobol/`. The journal's inputs were rebuilt with
GnuCOBOL's gl024/gl025/gl026/gl029 exactly as `batch.sh` does (those
take `ARGUMENT-VALUE`, after v1); our gl039 built the index.

What landed: `REPORT SECTION` with `RD` (`PAGE LIMIT`, `HEADING`,
`FIRST DETAIL`, `LAST DETAIL`), `TYPE PAGE HEADING` and `DETAIL`
groups, `LINE n` / `LINE +n` / `LINE PLUS n`, fields with `COLUMN`,
`PICTURE`, `SOURCE` (qualified), `VALUE`, `JUSTIFIED`, `BLANK WHEN
ZERO`; `INITIATE` / `GENERATE` / `TERMINATE`. Each `GENERATE` site
emits the fit test, the page advance with the heading rendered inline,
then the group's lines; `libcob` owns the page model
([report-writer.md](report-writer.md)). Refused with a message:
`CONTROL`, `SUM`, footing and heading types beyond PH, `GROUP
INDICATE`, `NEXT GROUP`, summary `GENERATE`, subscripted `SOURCE`.

## Stage 8 — SCREEN SECTION, first screen **M** — DONE 2026-08-29

- [screen.md](screen.md) against `term.h`
- `CBL_GET_SCR_SIZE`; `USAGE BINARY-CHAR [UNSIGNED]`
- The `PIC X(6)` → `PIC S9(3)V99 COMP-5` `MOVE` usescreen makes

Done: `usescreen.cbl` runs.

What landed: the SCREEN SECTION as slot tables (`screen.md`, "As
built"), `DISPLAY screen` and `ACCEPT screen` on the term service
with the dBase-Stage-4 focus loop, `CBL_GET_SCR_SIZE`, the
alphanumeric-to-numeric `MOVE` cell as GnuCOBOL measures it (blanks,
sign, digits, point), the main wrapper leaving through `cob_stop_run`
so the terminal is restored. `usescreen.cbl` compiles unchanged and,
with `42.25` typed into it on the emulator's stdin, paints
`$ 42.25` on line 3 under `slow32-fast` and `slow32-dbt`. The harness
gained `.keys` (typed into the program) and a `no oracle` marker;
`tests/free/screen.cbl`'s expected output is the ANSI stream,
reviewed by hand. The eventual target the user described (TAB order,
Enter as submit, numeric anchoring, AUTO, SECURE, underline / reverse
video) is written in `screen.md`; numeric anchoring, SECURE and
underline are not there yet.

## Stage 9 — the menu, and what it drags in **L** — DONE 2026-08-29

`menu.cbl` looks like a screen program. It is not small: it `CALL`s
`taskdt` (after the rewrite), and `taskdt.cbl` uses, on the gate
path,

- `EVALUATE … WHEN … WHEN OTHER … END-EVALUATE`, nested
- `STRING … DELIMITED BY SPACE / SIZE … WITH POINTER … END-STRING`
- `INSPECT … TALLYING … FOR LEADING ZERO`
- `INITIALIZE`
- **reference modification** with arithmetic:
  `todays-year(leading-zeros + 1:length(todays-year) - leading-zeros)`
- `FUNCTION LENGTH`, `FUNCTION CURRENT-DATE` (a clock, through MMIO),
  `FUNCTION UPPER-CASE` — all 1989 amendment, invoked with the
  `FUNCTION` keyword and no `REPOSITORY`
- `REDEFINES` of a `VALUE`d list as an `OCCURS` table
- a third screen, `date-page`, with `FROM` of a runtime-built item

So the Nucleus Level 2 verbs the earlier plan put "after v1" are in
v1, at the width taskdt uses them — and every one of them is 1985,
which is why they stay when `FUNCTION-ID` goes. Full-width
`INSPECT`/`STRING` (`REPLACING`, `CONVERTING`, multiple `INTO`s) can
still wait.

Done: `menu.cbl` runs, `DT` shows today's date.

What landed: `EVALUATE` (several subjects with `ALSO`, `TRUE`/`FALSE`
subjects with condition objects, `THRU`, `NOT`, `ANY`, stacked
`WHEN`s, `WHEN OTHER`, expressions as subjects, nested), `INSPECT
TALLYING` (`CHARACTERS` / `ALL` / `LEADING`, several tallies) and
`REPLACING` (`CHARACTERS` / `ALL` / `LEADING` / `FIRST`),
`INITIALIZE` (a template of the item's default initialisation with
`VALUE`s ignored, `memcpy`'d -- tables and groups included),
reference modification `item(start:len)` with literal or expression
bounds and the length omitted, usable as sender, receiver and
`STRING` source (a runtime descriptor of the slice), `FUNCTION
LENGTH` folded at compile time, `FUNCTION CURRENT-DATE` from the
guest clock, figurative `STRING` sources, `DELIMITED BY` defaulting
to `SIZE`. Staged operands now nest (an expression inside a
reference modification inside a `STRING` source stages above its
caller's slots). `menu.cbl` + `taskdt.cbl` + `clinkages.cbl` +
`dateutil.c` compile unchanged; with `DT`, Enter, `LO` typed, the
date page paints today's date -- `Saturday, August 29, 2026` -- under
`slow32-fast` and `slow32-dbt`, and `LO` leaves cleanly. Tests:
`nucleus2` (all of the above with taskdt's date build on a fixed
stamp, oracle-agreed) and `curdate` (the clock's shape). Not in:
`INSPECT ... BEFORE/AFTER INITIAL`, `CONVERTING`, `INITIALIZE
REPLACING`, `LENGTH` of a variable-length slice.

## Stage 10 — sequential V / RDW **M** — DONE 2026-08-29

- [framing.md](framing.md)
- Round-trip with tapemgr

Done: a V file this compiler writes is read by tapemgr; a V file
tapemgr writes (or cobc370's `vrec` output, ASCII-translated if
needed) is read back.

What landed: [framing.md](framing.md) "As built" -- the four V
spellings on the FD, the WRITE length from the 01 named or the
`DEPENDING ON` item, READ setting that item and leaving the tail,
status 44 past the bounds. The harness's `.tapemgr` step sends every
V file the test wrote through `tapemgr create` and `extract` and
demands the bytes back unchanged; both of vrec's files pass, so what
we write is what tapemgr reads, and what tapemgr writes is what we
read. GnuCOBOL's own V framing differs (recorded), so its output is
the oracle for the program's stdout only.

## Stage 11 — v1 close — DONE 2026-08-29

- Intrinsics at exactly the v1 set: `LOWER-CASE`, `UPPER-CASE`,
  `LENGTH`, `CURRENT-DATE`. Nothing else is referenced by a gate.
- `batch.sh`'s `run_cobol` switched from `cobcrun -M MAJESTY` to the
  emulator for the retired programs (see
  [majesty-corpus.md](majesty-corpus.md))
- Document remaining majesty programs as Stage 12+
- cobc370 still untouched

**v1 is done.** GnuCOBOL is retired for the journal and both charts
of accounts, and for the two screen programs.

**Closed 2026-08-29.** Intrinsics are exactly the v1 set. `batch.sh`'s
`run_s32` (majesty commit "batch: the chart-of-accounts and journal
reports run on SLOW-32") runs gl022, gl023, gl039 and gl030 on the
emulator in a private working directory, the other steps still under
`cobcrun`; the whole batch exits 0 and all twelve
`reports_cobol/*.prn` are byte-identical to a same-day run of the
untouched all-GnuCOBOL batch, with majesty's test suite giving the
same verdicts. The rest of the corpus is measured and ranked in
[majesty-corpus.md](majesty-corpus.md) "Stage 12+" (22 of 58 compile;
`COPY` blocks eight, then the legacy `FUNCTION-ID` date family, then
`ACCEPT FROM ARGUMENT-VALUE`). cobc370 is untouched -- the one
defect this work found outside its own tree was in tapemgr, and is
fixed there. The CCVS-85 histogram stays on the after-v1 list.

Stages 1 through 11 landed in one day, 2026-08-29: eleven commits,
about 6,000 lines of compiler and runtime, 33 tests with GnuCOBOL as
the oracle for every program that can run without a tty, and the
product claim -- the same reports, produced on this machine, without
GnuCOBOL -- true for the three reports and two screens v1 named.

## Stage 12 — COPY **S** — DONE 2026-08-29

The Library module: `COPY text-name [OF/IN library] [SUPPRESS].` is
replaced, period included, by the copybook's tokens before parsing --
so majesty's `fd glacct copy sglacct.`, where the copybook supplies the
FD's remaining clauses and its record, works as written. The copybook
is read in the same reference format, may COPY in turn (8 deep), and is
looked for as the name given, then `.cpy` / `.CPY` / `.cbl`, beside the
source and in the `-I` directories (`s32-cobc -I`, `compile.sh -I`,
majesty's `-I../copy`). Diagnostics name the copybook. `REPLACING`
is refused for now (nothing in the corpus uses it). Seven of the eight
blocked programs compile; 29 of 58 now.

## Stage 13 — the command line **S** — DONE 2026-08-29

`ACCEPT x FROM ARGUMENT-NUMBER` (the count, excluding the program
name), `ACCEPT x FROM ARGUMENT-VALUE` (the arguments in turn from 1;
past the end the item is left alone), `DISPLAY n UPON ARGUMENT-NUMBER`
(the next one is n), `ACCEPT x FROM COMMAND-LINE` (the arguments
joined by blanks) -- GnuCOBOL's implementor module, measured and
matched; the main wrapper hands crt0's argc/argv to `libcob` first
thing. The harness passes a `.args` file to the program and the
oracle. **gl024, the journal pipeline's first step, compiles unchanged
and, run in place with this month's `YYYYMM`, writes
`transactions-sel.txt` and `transactions-ids.txt` byte-identical to
GnuCOBOL's** (213 lines each); gl038 compiles too. Two compiler slips
the sweep caught: an out-of-line `PERFORM para` swallowed the
enclosing inline PERFORM's `END-PERFORM` (gl042, gl043), and a report
field with `VALUE` but no `PICTURE` was refused (gl036).

**The journal pipeline end to end, on SLOW-32.** gl024 → gl025 →
gl026 → gl029 → gl039 → gl030, with `batch.sh`'s host sorts between,
run in place against majesty's data: every intermediate --
`transactions-sel.txt`, `transactions-ids.txt`, `lines-sel.txt`,
`lnaccts.txt`, `txnlnaccts.txt` -- byte-identical to GnuCOBOL's run of
the same pipeline, then both journals identical, under `slow32-fast`
and `slow32-dbt`. majesty's `batch.sh` now runs that whole pipeline
under `run_s32` in its private `tmp/` (majesty commit "batch: the
whole journal pipeline runs on SLOW-32"; `s32x/build.sh` builds all
eight binaries); the whole batch exits 0 with all twelve reports
identical to the all-GnuCOBOL baseline.

## Stage 14 — OCCURS DEPENDING ON **S** — DONE 2026-08-29

`OCCURS m TO n [TIMES] DEPENDING ON d` (with `ASCENDING KEY` and
`INDEXED BY` alongside): laid out at n, the 1985 rule for a receiving
item; d resolved to an integer item outside the table; entries used
by subscript, index or `PERFORM VARYING` exactly as fixed tables are.
A `MOVE` of a *group* whose length the table makes variable is refused
with a message (nothing in the corpus does it; one occurrence of the
table, always subscripted, is fixed-length and moves). `OCCURS
UNBOUNDED` is refused as 2002. gl034 and gl040 compile; gl034 stops
next at `SEARCH ALL`.

**The balances pipeline through gl041, on SLOW-32:** gl037 → gl038 →
gl040 → gl041 with `batch.sh`'s sorts between, run in place: 55,232
`translines`, 12,564 `balances`, 12,566 `balances-re` (gl040's ODO
table doing the retained-earnings work) and 12,566 `acctbal` rows,
each file byte-identical to GnuCOBOL's. gl042/gl043, which print the
balance sheet and profit-and-loss from `acctbal`, wait on `SEARCH`.

## Stage 15 — SEARCH **S** — DONE 2026-08-29

`SEARCH table [VARYING id] [AT END s] {WHEN cond s}... [END-SEARCH]`
walks the table's first `INDEXED BY` item from its current value;
`SEARCH ALL` sets it to 1 first. Both are index scans: every `SEARCH
ALL` in the corpus is over a table ordered by its key with unique
keys, where the first entry satisfying the `WHEN` is the one a binary
search would report. The bound is the `OCCURS` count or the
`DEPENDING ON` item. The `AT END` and `WHEN` bodies are parsed once
and emitted after the loop. Two Report Writer rules came out of gl043
(report-writer.md, "Two rules measured later"). gl034, gl042 and gl043
compile -- gl042/gl043 after their `EXIT PARAGRAPH` (2002) became `GO
TO process-class-exit` in majesty, verified under GnuCOBOL -- and with
gl033, gl035 and gl036 the activity pipeline runs too: txnlines
(55,232), txnlines-re (56,164, `SEARCH ALL` over the ODO table) and
acttxnlin (56,164) byte-identical to GnuCOBOL's. **All twelve of
majesty's reports now come off SLOW-32 byte-identical**, the last two
(activity) after the spill rule above.

## Stage 16 — SPECIAL-NAMES CLASS, console ACCEPT **S** — DONE 2026-08-30

Picked from ISSUES-3: the only `SPECIAL-NAMES` clause the corpus uses
is a user-defined class (`class digits is '0' through '9'`, four
times in damm with different ranges). A class is a 256-entry table
in the literal pool, per program unit, and `x IS class` / `NOT class`
sits in `parse_simple` beside `NUMERIC`; `cob_class_user` walks the
item's bytes. damm's next stop was `ACCEPT ws-code` from the console
-- one line of standard input, moved as text (`cob_accept_console`)
-- and `LENGTH OF item`, the IBM register, folded like `FUNCTION
LENGTH`. The harness now feeds a test's `.keys` file to the oracle's
program as well as to ours. damm (four units, main + damm3/4/10)
compiles and its output is byte-identical to GnuCOBOL's over seven
inputs including the account-number fixtures majesty's own tests use
(one valid, one with a wrong check digit). Tests: free/classcond, free/accept
(both "oracle agrees"). 37 of 58 compile; gl008's first refusal turns
out to be a subscripted `SOURCE` in a report line, not SPECIAL-NAMES
(ISSUES-19 / GitHub #9 names it).

## Stage 17 — a subscripted SOURCE in a report line **S** — DONE 2026-08-30

GitHub #9 (ISSUES-19), gl008's first refusal: `source is
tax-tax(taxcode-index)`. A report field used to keep its SOURCE as a
name string and look it up at GENERATE; now it keeps the token
position and `parse_ref` reads the reference there, so subscripts,
qualification and reference modification are the same code as
everywhere else. Test free/rptsub generates from an ODO table with
`nm(i)(1:3)` on a field, oracle agreeing. Also refused now:
`REDEFINES FILLER` (the text names a data-name; GnuCOBOL rejects it;
we had taken it silently). gl008 next stops at `ROUNDED MODE`, a 2002
form majesty writes out as half-to-even arithmetic, then its table `SORT`
(GitHub #10, a product ruling).

## Stage 18 — the calendar functions **S** — DONE 2026-08-30

ISSUES-6. `INTEGER-OF-DATE`, `DATE-OF-INTEGER`, `DAY-OF-INTEGER`,
`INTEGER-OF-DAY` from the 1989 addendum, the same addendum
`CURRENT-DATE` came from. Integer 1 is 1601-01-01 (proleptic
Gregorian, the civil-from-days algorithm in `libcob.c`); an invalid
date or day gives 0. A numeric function result is a new shape on the
intrinsic path: the runtime renders it as unsigned DISPLAY digits in
its buffer and the compiler pairs it with a numeric descriptor
(`num_desc`), so MOVE, DISPLAY and the rest see an ordinary numeric
item. Widths when DISPLAYed directly follow GnuCOBOL (10 / 8 / 7),
measured, since the standard leaves them to the implementor. Test
free/datefn, oracle agreeing; jerm2 -- majesty's own cross-check of
the C `du_*` date routines against these functions over 400,000 days
-- compiles, runs in 0.4 s under the DBT, and disagrees nowhere on
either engine (its output window differs by the container's UTC day).
38 of 58 compile.

## Stage 19 — relative I-O **M** — DONE 2026-08-30

ISSUES-1, the first item that was a module rather than a clause.
Measured first: three probe programs under the GnuCOBOL container
gave every status (22 occupied, 23 absent, 24 for key 0, 43 with no
prior READ, 10 at end), READ NEXT skipping empty slots and setting the
key item, START leaving it alone, and the bytes on disk (an 8-byte
native length per slot). Built: slots of `4 + recsize` framed with the
mode-V RDW (docs/indexed.md "As built"), which made empty slots and
`RECORD CONTAINS 10 TO 98` -- glentry's control record and data
records -- fall out of the same frame. Runtime: `rel_slot_get/put`
and the six verbs in libcob.c; `cob_file` grew the key item, its
descriptor and two counters. Compiler: `RELATIVE KEY` in SELECT
(refused inside the record, or without ORGANIZATION RELATIVE, or
missing under random/dynamic access), the verbs' relative forms,
INVALID KEY on WRITE/REWRITE/DELETE/START. tests/free/relative --
random, dynamic and sequential access in one program -- agrees with
GnuCOBOL line for line; bad/relative-key-in-record replaces the old
"not implemented" refusal. crglentry then exglentry run on SLOW-32
with output identical to GnuCOBOL's. 40 of 58 compile; ldglentry is
now an `SD` program (ISSUES-4).

## Stage 20 — gl008 arrives; branch relaxation **S** — DONE 2026-08-30

The corpus's biggest program. Its 2002 forms were rewritten in majesty
under the standing ruling (table `SORT` as stable insertion sorts,
`ROUNDED MODE NEAREST-EVEN` as half-to-even written out in 85 arithmetic
(first as plain `ROUNDED`, which the user reversed: a half-cent always
moved the same way; majesty 7f2d3ce / 06f5cc1), a subscripted subscript as
two MOVEs; dist01's `OCCURS UNBOUNDED` and 21-digit item likewise),
verified first under GnuCOBOL -- old and new gl008 print twelve
receipts byte-identically -- and then the compile stopped in the
assembler: a PERFORM body longer than a conditional branch's ±4096
bytes. The compiler now buffers its assembly and relaxes such
branches (ISSUES-20; tests/free/farbranch). gl008 then runs on
SLOW-32 with all twelve receipts identical to GnuCOBOL's, and
`run_gl008.sh` uses it. 42 of 58 compile; what remains is `SD`/`SORT`
(glacpost, ldglentry), the date family to retire, and three
extension programs.

## Stage 21 — file SORT **M** — DONE 2026-08-30

ISSUES-4, the Sort-Merge module's file form: `SD`, `SORT ... ON
ASCENDING/DESCENDING KEY ... [WITH DUPLICATES IN ORDER] {USING |
INPUT PROCEDURE} {GIVING | OUTPUT PROCEDURE}`, `RELEASE`, `RETURN ...
AT END`. The SD is a `cob_file` of organization SORT; the statement's
records live in memory behind it (`cob_sorter`), ordered by a merge
sort on an index array -- stable, so DUPLICATES IN ORDER is the only
behaviour. USING and GIVING go through the other files' own READ and
WRITE, so a line-sequential input and a fixed-sequential output keep
their framings. The key table is emitted into .data beside the unit's
files; the procedures are PERFORMed through `emit_body`. tests/free/
sortfile (both forms, two keys in opposite directions, RETURN INTO),
GnuCOBOL agreeing. glacpost -- a SORT USING/GIVING then a master-file
update -- is byte-identical to GnuCOBOL on stdout and both files, and
crglentry → ldglentry (INPUT/OUTPUT PROCEDURE writing the relative
file) → exglentry is identical on well-formed input. With Kagura's
conversion of the date family (majesty e69e98b), **55 of 58**
programs compile; the three left are a `BINARY-INT` rewrite and the
XML/JSON extension pair.

## Stage 22 — CCVS-85 as a histogram, first pass **L** — 2026-08-30

ISSUES-17, and the first work under the "cobol/ does the language"
ruling. `tests/ccvs-histogram.sh` feeds the NIST modules (NC SQ RL IX
ST SM IC RW; IF once extracted) to `s32-cobc` and ranks first
refusals, the same exercise cobc370's `cobc-ccvs` does. The first run
was **4 of 303**, 285 of them stopped by one reader gap -- column-7
continuation lines. Four batches later, **202 of 303** compile; what
landed, each because it topped a bin: continuation lines; numeric
literals beginning with `.`; `ADVANCING PAGE`; `I-O-CONTROL`;
`SPECIAL-NAMES` switches, alphabets, device mnemonics; the `SIGN`
clause with a leading overpunch in the runtime; `DECLARATIVES` with
`USE AFTER ERROR PROCEDURE` (a dispatch after every I/O statement,
the runtime choosing the section by file or open mode); optional
`AT`/`KEY`/`ORGANIZATION IS`/`KEY IS`; `77 ... REDEFINES`; `SIGN` on a
group; `88 VALUE ALL`; `MERGE`; `CLOSE REEL`; qualified and repeated
paragraph names; `EVALUATE` with a condition subject; `PERFORM
x(i) TIMES`; a MOVE of an ODO group (the 85 receiving rule, a
documented divergence). "Compiles" here means s32-cobc produced
assembly; running the modules against their own PASS/FAIL output is
the next gate. Two bugs the suite found in what majesty never touched:
`GO TO` swallowing a following `NOT`, and a picture split across a
continuation line.

## Stage 23 — CCVS-85 run and scored **L** — 2026-08-30/31

`tests/ccvs-run.sh` runs what compiles and reads each program's own
report (`nnn OF nnn TESTS WERE EXECUTED SUCCESSFULLY`, the lines
GnuCOBOL's report.pl reads), one directory per module in the suite's
order with the X-card files fresh for each program, compile-only
programs counted as report.pl counts them. First run: 3535 of 3725
tests, 130 programs matching GnuCOBOL's tally. Every failure came with
a COMPUTED/CORRECT pair, and the fixes were: numeric VALUE images
with P positions and SIGN clauses; alnum-vs-numeric comparison as
characters; the insertion `0`; `*` check protection of zero; SEARCH
VARYING; the file-status matrix (05/35 on OPTIONAL and EXTEND, 38 after
CLOSE WITH LOCK, 43/44 on REWRITE, 46 after AT END, 48 for WRITE under
I-O, 07 for CLOSE REEL, 21 for an indexed sequential-access WRITE out
of order, 14 for a relative record number the key cannot hold); libcob
keeping the sequential file position itself, because the guest libc's
buffered stream reads ahead and REWRITE seeks by it; exact decimal
long division; the program collating sequence; SAME RECORD AREA.
**3741 of 3751 tests pass, none fail; 185 of 187 programs that run
match GnuCOBOL exactly** (the two short are NC121M/NC220M, whose
missing tests are console inspections). What does not compile is
listed in ISSUES-17.

## Stage 24 — ALTERNATE RECORD KEY **M** — DONE 2026-08-31

ISSUES-12, the IX module's other half. One sorted table per key,
entries of key bytes, slot and an arrival counter (the order
duplicates come back in); the key file grows to `S32KEY02` and still
reads `S32KEY01`, rebuilding the alternate tables from the records.
A random READ or a START names its key, which becomes the key of
reference for READ NEXT; a START on an item that begins where a key
begins is a START on that leading part; WRITE and REWRITE keep the
duplicates rule (22, or 02 when duplicates are allowed), REWRITE moves
a changed alternate, DELETE forgets every table. Also on the way:
`READ file` under DYNAMIC access is by the prime key, a qualified
`RECORD KEY IS k IN group`, `CLOSE f LOCK` without WITH. free/altkey
against GnuCOBOL: identical but for the 02 on the first record after
a START, where the text is followed (`.oracle-expected`). NIST IX:
29 of 29 programs, 438 of 439 tests, all matching GnuCOBOL's tally;
the suite as a whole 4050 of 4061, none failing.

## Stage 25 — LINAGE **S** — DONE 2026-08-31

The print file's logical page, built to GnuCOBOL's `cob_linage_write_opt`
so the bytes and the counter agree with the oracle: `lin_write` in
libcob.c, the values re-taken at each page; `LINAGE-COUNTER` is a
cell of the file's `cob_file` image that the compiler declares as a
77 for each LINAGE file (`LINAGE-COUNTER OF file` when there are
several); `WRITE ... [NOT] [AT] END-OF-PAGE`/`EOP` reads the runtime's
verdict into the same slot the other clauses use. free/linage, oracle
agreeing on counters, verdicts and the page read back; NIST SQ201M,
SQ208M-SQ210M compile and match GnuCOBOL's tally.

## Stage 26 — COPY REPLACING and REPLACE **S** — DONE 2026-08-31

The Source Text Manipulation module's other half. `==` is a token of
the tokenizer's own (the picture scanner stops at it; a period before
it is the separator); `expand_copies` parses the REPLACING pairs --
pseudo-text, literal, word, or an identifier with its qualifiers and
subscripts (SM206A's `BY x IN y IN z (1)`) -- and rewrites the
copybook's token vector; `apply_replace` runs the same match over the
whole stream once every COPY is in, `REPLACE OFF` or the next
`REPLACE` ending the reach of the pairs. What the CCVS insisted on:
a debugging line is text for the matching and a comment afterwards
(KP008's `D THIS IS GARBAGE.` sits inside the pseudo-text that must
match), a `COPY` on a debugging line stays a comment (SM101A), and
the nesting guard counts depth, not expansions (SM101A copies 116
times). free/copyrep, free/replace; SM 12 of 13 compile and every
one matches GnuCOBOL's tally; the whole suite 217 of 303 compile,
4115 of 4152 tests pass, none fail, 214 programs match.


After Stages 12–15, majesty's `batch.sh` runs **every COBOL report
step on SLOW-32**: the charts, the journal pipeline, the balances
pipeline (gl037–gl043) and the activity pipeline (gl033–gl036), each
pipeline in its own working directory with its own description index
(majesty commit "batch: every COBOL report step runs on SLOW-32").
Measured: exit 0, all twelve `reports_cobol/*.prn` byte-identical to
the all-GnuCOBOL baseline, `tmp/` clean; eighteen binaries from
`s32x/build.sh`. What still runs under `cobcrun`: the interactive
`menu` (which runs on SLOW-32 too, but not through a pipe) and
`today`, whose output matches GnuCOBOL's to the hundredth of a
second. 36 of 58 corpus programs compile; what is left is the legacy
`FUNCTION-ID` date family (retire), relative I-O, `SPECIAL-NAMES`,
`SORT`, and one-offs.

## After v1 (not scheduled, each when a program asks)

The ranked, maintained form of this list is [../ISSUES.md](../ISSUES.md)
(2026-08-30); what follows is the list as written at v1.

- Rest of majesty batch (`gl024`–`gl043`, relative I-O, `w001`)
- In-program `SORT` (`dist01`, `gl008`, `glacpost`, `ldglentry`)
- Nucleus Level 2 at full width (`CORRESPONDING`, abbreviated
  conditions, nested programs, `REPLACE`, `INSPECT REPLACING`)
- The rest of the corpus rewrite: the pure-COBOL date-function family
  (`fielded_to_linear.cbl`, `linear_to_fielded.cbl`, `isvaliddate`,
  `isleapyear`, `floor-div`, `floor-divmod`, `holidays`' inner units)
  is still `FUNCTION-ID`; its invocations sit inside arithmetic and
  conditions and need hoisted temporaries, and nothing on `batch.sh`
  reaches them -- do it when `jerm`/`exgltrans` are compiled, with a
  test plan of its own
- Report Writer `CONTROL`/`SUM`
- Alternate keys
- dBase-compatible writer filter
- CCVS-85 NC/SQ/IC as a pass/fail suite
- `rs.c`, `csvgen.c`, `crc.c` on SLOW-32, when a program calls them

Sort-Merge as a module, Debug, Communication stay out.

## What not to start a stage with

- Copying `fortran/src/hir_*.h`
- Copying `cobc370.c`
- A dBase-openable file as the first indexed test
- Report Writer `SUM` before gl030 prints
- SSA
