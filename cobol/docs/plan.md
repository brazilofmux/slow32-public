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

## Stage 27 — DECIMAL-POINT IS COMMA **S** — DONE 2026-08-31

The last SM program. The clause reaches SM103A by `COPY K3SNA`, after
the main text has been scanned, so it is not a tokenizer mode: a comma
tight between digits becomes a token of its own, and once COPY and
REPLACE are done `apply_decimal_point` looks for the clause, joins
`12345678,91` into one literal and swaps `.`/`,` in every picture so
the rest of the compiler reads the ordinary form. libcob keeps
`cob_dp_comma` (set on entry, restored on exit, frame slot 100) and
swaps the characters after `cob_edit_apply`, before `cob_deedit`, and
in DISPLAY of a scaled item; the compiler does the same for a literal
displayed as written. free/dpcomma (edited pictures with `.`
grouping, floating `-`, currency, de-editing, COMPUTE with `0,25`,
DISPLAY of `3,75`), oracle agreeing; SM 13 of 13 match GnuCOBOL; the
suite 218 of 303 compile, 4121 of 4158 pass, none fail, 215 match.

## Stage 28 — CALL identifier, ON EXCEPTION, CANCEL **M** — DONE 2026-08-31

The IC module. First a runner finding: report.pl compiles `IC/lib`
(the CALLed subprograms) before the mains and ccvs-run.sh never did,
so every IC program failed at the link and the histogram bin read as
"nested programs" -- building `lib/` once per module and linking it
in made IC 0 → 11 with no compiler change. Then the language: every
unit emits a registration stub in `.init_array` (runtime/start.c runs
them before main) giving libcob its PROGRAM-ID, entry and a CANCEL
routine; `CALL identifier` resolves through that registry into r12
(callee-saved, unused by the compiler) and `jalr`s; `[NOT] [ON]
EXCEPTION|OVERFLOW` is the lookup failing, so a literal CALL with the
clause is routed through the registry too; `CANCEL` copies each
WORKING-STORAGE record's initial image (kept in `.rodata`) back over
it, which is what IC203A's "SET TO INITIAL STATE" tests measure. A
subtlety on the way: `ADD 2 TO X NOT ON EXCEPTION` inside a CALL's
branch -- the arithmetic clause parser must claim `NOT` only before
`[ON] SIZE`. free/dyncall (oracle agreeing, after two GnuCOBOL-isms
were written out of the fixture: DISPLAY's own 2002 exception clause,
case-sensitive names); IC 16 of 25, 125 of 125, all matching; the
suite 233 of 303 compile, 4245 of 4282 pass, none fail, 230 match.


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

## Stage 29 — contained programs, GLOBAL, USE GLOBAL **M** — DONE 2026-08-31

The nesting the IC module is named for. The compiler kept one symbol,
file and paragraph table per unit and reset them between sequential
units; now the tables are shared with a per-unit base, and an
`IDENTIFICATION DIVISION` met where a sentence would begin pushes the
containing unit's state (`UnitSave`: bases, PROGRAM-ID, SPECIAL-NAMES
tables, USE entries, sort tables) and compiles the contained program
as a unit of its own, cutting the tables back on its `END PROGRAM`.
Files and paragraphs carry their unit so labels resolve across the
boundary (`.Lf<unit>_<i>`, `.Lp<unit>_<id>`; paragraph ids were
already file-unique, which keeps the runtime PERFORM stack honest).
Name lookup falls outward to GLOBAL items -- the flag propagated down
from a GLOBAL 01 or FD once the tree is built -- and GLOBAL files. The
USE decision moved from the runtime (which chose by the file image's
own unit) into the compiler: after each I/O statement it emits the
candidates in the standard's order, the program's own for the file,
then the open mode (`cob_open_mode`), then each containing program's
GLOBAL ones; the runtime's result code (2 with a FILE STATUS, 3
without) says whether `cob_io_unhandled` stops the run when nothing
matched. Two things the tests taught: a contained unit's data
emission leaves the assembler in `.data`, so the containing program's
epilogue had to be put back in `.text`; and the contained unit must
append its USE entries after the enclosing units', not restart the
table (IC234A's three levels). `IS INITIAL` now re-initialises through
the CANCEL image; `IS COMMON` is accepted. free/nested (two levels,
GLOBAL data, INITIAL, same-named private items), oracle agreeing;
free/nestuse (a GLOBAL file, the READ at end in the innermost program
reaching the outermost USE GLOBAL past a non-matching one) has no
oracle: GnuCOBOL 4.0-early-dev never returns from a containing
program's USE procedure invoked for a contained program's I/O -- it
hung the harness twice, which now puts a timeout on every oracle run
-- while IC233A/IC234A, which GnuCOBOL does pass, cover the rule and
match ours. IC 20 of 25, 143 of 143, all matching; the suite 237 of
303 compile, 4263 of 4300 pass, none fail, 234 match.

## Stage 30 — EXTERNAL **S** — DONE 2026-08-31

Data shared by name between separately compiled programs. An EXTERNAL
record is a cell-addressed record like a LINKAGE one, filled at entry
from `cob_external(name, size)` -- one zeroed block per name for the
executable, grown if a later program declares it longer. An EXTERNAL
FD is one connector: `cob_ext_file_enter(name, image, recblock)` lends
the first program's image to everyone (its record pointer is the
block shared under `file:<name>`, which every program's FD records
address), and swaps the connector's FILE STATUS pointer to the
entering program's item, `cob_ext_file_exit` swapping it back -- what
IC227A's F-S-PARAM test measures, since its subprogram's status item
is in the LINKAGE SECTION, which a FILE STATUS may now be (the image
takes the address at entry, status at offset 16). free/external (a
shared item both ways, a file written by two programs and read back,
the two status items), oracle agreeing; IC226A and IC227A's EXTERNAL
tests pass (IC227A goes on to BY CONTENT); IC 21 of 25, 147 of 147;
the suite 238 of 303 compile, 4267 of 4304 pass, none fail, 235 match.

## Stage 31 — BY CONTENT; the IC module complete **S** — DONE 2026-08-31

The last IC bin. `BY CONTENT` hands the callee a copy from a runtime
arena that behaves as a stack: `cob_content_push(p, n)` before the
CALL for each such argument (an item or a literal; the `Arg` kind
A_CONTENT stages the copy's address), `cob_content_pop(k)` after it
with the result kept aside, so nested and recursive calls balance.
Numeric literals as CALL arguments, by reference or by content, are
now their plain digits rather than the pool's sign-led image (the
callee reads them through its own picture; GnuCOBOL hands over a
binary word, a documented divergence). The runner compiles and assembles the suite's compile-only
programs without linking, since IC401M CALLs a program the suite never
supplies. free/bycontent (copy vs reference, literals), oracle
agreeing; **IC 25 of 25 compile and match GnuCOBOL, 247 of 251**; the
suite 242 of 303 compile, 4367 of 4408 pass, none fail, 239 match.

## Stage 32 — Report Writer at 85 width; the RW module complete **M** — DONE 2026-08-31

The NIST RW programs wanted the page half of the module as the text
writes it, not as majesty's reports happen to: a flat group parser
(clauses in any order at any level; LINE begins a line, the printable
clauses make a field, one entry may be both -- the elementary group),
`TYPE PAGE FOOTING` and RD `FOOTING`, `LINE-COUNTER`/`PAGE-COUNTER`
as items of the report block (INITIATE 0 and 1; the first GENERATE
begins page 1 without counting it -- `page_started` now separate from
the counter; LINE-COUNTER set to a line's number before its SOURCE
items move, so the PH line prints 1), an RD without PAGE as one
endless page, and a record-oriented print file. The runner's
compile-only list gained RW301M/RW302M, SQ303M and DB205A from
report.pl. free/rwpage (counters after INITIATE, GENERATE and
TERMINATE, the footing, a page break at LAST DETAIL): stdout agrees
with GnuCOBOL; its `.prn` rendering of the detail line is garbage, so
the print file is ours by the text (report-writer.md). RW 6 of 6
match, 42 of 42; the suite 248 of 303 compile, 4409 of 4450 pass,
none fail, 245 match -- IC and RW complete.

## Stage 33 — the ODO table at any depth **S** — DONE 2026-08-31

A MOVE of a group whose OCCURS DEPENDING ON table sits in a subgroup
(NC247A, SQ214A and ST146A: `03 GRP. 04 T OCCURS 0 TO 9 DEPENDING ON
D`): the sending length is size - (max - d) x element whatever the
depth, the layout being at the maximum -- `odo_table_below` finds the
table, and the group's shape is checked so that nothing follows it
(items after an ODO table sit at variable locations in the 85 text;
this layout does not slide them, so such a MOVE is refused by name
rather than sent wrong). free/odonest, oracle agreeing (GnuCOBOL
refuses the variable-location shape outright); SQ214A and ST146A
match GnuCOBOL, NC247A reaches UNSTRING; the suite 250 of 303
compile, 4418 of 4459 pass, none fail, 247 match.

## Stage 34 — UNSTRING; an ODO group's current length everywhere **S** — DONE 2026-08-31

The last verb of the 85 nucleus that was refused by name. The runtime
scans (`cob_unstr_begin/delim/into/pointer/tally/overflow`, the shape
of STRING's), the compiler parses the whole statement and emits one
call per receiver with its DELIMITER IN and COUNT IN; what the text
insisted on: with no DELIMITED BY a receiver takes as many characters
as it holds, an empty field is spaces or zero, TALLYING is
incremented. NC218A's 125 tests match GnuCOBOL. NC247A then showed
the rule Stage 33 gave MOVE alone applies to every operand use of a
group over an ODO table -- IF, STRING, UNSTRING, INSPECT: such an
operand now becomes `(1:current length)` at parse time (`Ref.rm_odo`,
the length computed by `cob_odo_length` where reference modification
computes its own), so one mechanism serves them all and receivers keep
the maximum. free/unstring, oracle agreeing; NC 53 of 95, 2176 of
2181; the suite 252 of 303 compile, 4563 of 4605 pass, none fail, 249
match.

## Stage 35 — INSPECT as the text describes it **M** — DONE 2026-08-31

BEFORE/AFTER INITIAL was the ask; the four programs then taught the
rest. The phrase itself is a range per phrase, found in the item's
original contents. But a fixture written for it disagreed with
GnuCOBOL on a two-phrase TALLYING, and the text is with GnuCOBOL: one
INSPECT is one pass, the phrases tried in order at each position, a
matched position taken from every later phrase. The runtime was
per-phrase; it now registers the phrases (`cob_inspect_phrase`, with
`cob_inspect_range` before each that has one) and makes the pass
(`cob_inspect_run`), the compiler adding the counts after
(`cob_inspect_count`). The first single-pass build regressed twelve
NIST tests -- a statement with both TALLYING and REPLACING is two
statements, the tallying pass first -- and NC216A added CONVERTING
(one single-character phrase per character), operand lists under one
ALL/LEADING, and the signed DISPLAY item inspected without its
embedded sign. free/inspinit, oracle agreeing; NC115A, NC122A, NC216A,
NC221A match; the suite 256 of 303 compile, 4692 of 4734 pass, none
fail, 253 match.

## Stage 36 — CORRESPONDING **S** — DONE 2026-08-31

MOVE, ADD and SUBTRACT CORRESPONDING, natively: `corr_walk` pairs the
two groups' subtrees at compile time by the 6.4.2 rules (same name and
qualifiers, no FILLER, no REDEFINES/OCCURS with their subtrees, groups
that correspond searched further), emits a MOVE per pair with at least
one elementary item, or the arithmetic per pair of elementary numeric
items through the existing push/store path with ROUNDED on each and
the size-error flag OR-ed across the pairs into one ON SIZE ERROR. The
operands' subscripts carry to every pair by copying the Ref. NC207A
then wanted 48 qualifiers on one name (`GROUP-49-1 OF GROUP-48 IN ...`);
the cap was 8, now 64. free/corr, oracle agreeing; NC202A, NC207A,
NC208A, NC222A, NC253A match; the suite 261 of 303 compile, 4947 of
4989 pass, none fail, 258 match.

## Stage 37 — PICTURE at 85 width **S** — DONE 2026-08-31

Three faults behind six refusals: the tokenizer folded a separator `;`
or `,` after a picture into it; `pic_analyse` refused `A`/`X`/`9`
mixed (they are alphanumeric, X3.23 5.3.9) and `P` beside `Z`; and
its stored-digit count knew only `9`, so `ZZZPP` read as leading P.
The editor's digit string now leaves the P positions out, DISPLAY of
a trailing-P item shows the value with its low zeros, and
`cob_put_num` scales a value up without passing 64 bits (NC104A's
12345 into `9V9(17)` came out 4.318529385). free/picmix, with
GnuCOBOL's own answers for `ZZZPP` beside the text's (4.0-early-dev
scales by the P count twice); tests/pictures.expected re-pinned for
`Z9P` and `X9`; NC104A matches; the suite 262 of 303 compile, 5088 of
5130 pass, none fail, 259 match. The six programs go on to their next
refusals: a non-integer numeric MOVEd to an alphanumeric item,
REMAINDER with a ROUNDED quotient, RENAMES, USAGE on a group.

## Stage 38 — DIVIDE ... REMAINDER at full width **S** — DONE 2026-08-31

The remainder is defined on the quotient as it would be stored before
ROUNDED (6.9.4); it was computed from the stored quotient, which
broke under ROUNDED, so it is now recomputed: the quotient truncated
to the receiver's decimals (`cob_ntrunc`), times the divisor, from
the dividend. ON SIZE ERROR with REMAINDER: the clause is looked for
past the REMAINDER phrase so the quotient's store knows of it, a
quotient overflow (the flag in SLOT_B) skips the remainder's store,
and the remainder's own overflow joins the flag. A numeric-edited
remainder receiver is allowed. free/remrnd, oracle agreeing; NC203A
and NC251A match; the suite 264 of 303 compile, 5204 of 5246 pass,
none fail, 261 match.

## Stage 39 — RENAMES **S** — DONE 2026-08-31

Level 66 as a Sym outside the record's tree: `build_tree` gives it the
01 it follows as parent without linking it as a child, so layout,
group MOVE and CORRESPONDING never see it, while `OF record` and the
storage emission (which skips subordinates) work unchanged; after
layout its two names are resolved with the 01's name as an implicit
last qualifier (NC252A writes both `RENAMES X OF REC` and bare names
that repeat across records) and it takes `a`'s offset to the end of
`b`, an alias of `a` when `a` alone is elementary, else a group.
NC252A's two remaining failures were not RENAMES: a COMPUTE into
`S99P` overflowed silently because the size-error test counted P as
a digit -- the descriptor of any item with P in its picture now
carries the picture, and `cob_put_num` tests and truncates on the
stored digits. free/renames, oracle agreeing (GnuCOBOL wants the
repeated names qualified; ours need not); NC252A matches; the suite
266 of 303 compile, 5311 of 5353 pass, none fail, 263 match.

## Stage 40 — USAGE on a group **S** — DONE 2026-08-31

`build_tree`'s second pass hands a group's USAGE down to each child
that has none, before the children are finished, so `01 G USAGE COMP`
lays its pictures out binary and `01 G USAGE IS INDEX` makes
picture-less children index items (NC131A, NC135A); a child that
contradicts the group is refused, as GnuCOBOL refuses it. NC135A's
eighth "test" turned out to be report.pl inspecting the 20x15 table
the program prints; ccvs-run.sh now scores it the same way.
free/grpusage, oracle agreeing; NC131A, NC135A, NC245A match; the
suite 269 of 303 compile, 5357 of 5399 pass, none fail, 267 match.

## Stage 41 — VARYING ... AFTER at 85 width **S** — DONE 2026-08-31

The limit of three levels was an array; it is eight. The fixture for
four levels then disagreed with GnuCOBOL twice, and the text was with
GnuCOBOL both times: with TEST BEFORE an inner item is set back to its
FROM when its condition comes true and the outer item is augmented
(6.20.4), so the inner items read FROM at the end -- `emit_varying`
now resets an inner level as its loop ends; and WITH TEST AFTER across
levels, which had been refused, follows the figure for two
identifiers: the innermost condition after each body, then outward,
a false outer condition sending every inner item back to FROM. NC201A
had been the program refused for that; free/vary4, oracle agreeing;
NC201A, NC233A and NC243A match; the suite 272 of 303 compile, 5447
of 5488 pass, none fail, 270 match.

## Stage 42 — CLASS literals, switches from the environment **S** — DONE 2026-08-31

`CLASS name IS "ABCD"` puts every character of the literal in the
class (only one-character literals were taken). The two programs then
wanted two more switch things: `SET SW-1 TO ON SW-2 TO OFF` -- several
mnemonic groups in one SET -- and switches that are on at start:
report.pl runs every program with COB_SWITCH_1=ON and COB_SWITCH_2=OFF
in the environment and NC254A checks them, so `cob_init` reads
COB_SWITCH_1..8 (the emulator passes the host environment through)
and ccvs-run.sh sets the two as report.pl does. free/classlit, oracle
agreeing; NC174A and NC254A match; the suite 274 of 303 compile, 5533
of 5574 pass, none fail, 272 match.

## Stage 43 — CURRENCY SIGN, and what NC107A/NC108M wanted next **S** — DONE 2026-08-31

CURRENCY SIGN IS "c" is applied where DECIMAL-POINT IS COMMA is: once
the text is whole, `c` in every picture becomes `$`, and the runtime
swaps `$` for `c` after editing and back before de-editing
(`cob_set_currency`, saved and restored per unit; the frame grew to
112 for the slot). NC108M then wanted BLANK WHEN ZERO on a plain
numeric item (the runtime honoured it only in edited pictures), and
NC107A wanted paragraphs named `3`, `4`, `5` -- procedure-names of
digits only, recognised in the prescan, the statement loop, GO TO and
PERFORM -- which was also NC114M's "`0` statement". The runner learned
two things from NC107A: BSD awk stops a line at the NUL bytes
LOW-VALUE prints, so the summary is scanned with NULs stripped; and
report.pl scores the five "*** INFORMATION ***" lines by their bytes
(ZERO, SPACE, QUOTE, HIGH-VALUE, LOW-VALUE as 20 characters), which
ccvs-run.sh now does in perl. free/currency, oracle agreeing; NC107A
(177 tests) and NC108M match; the suite 276 of 303 compile, 5724 of
5765 pass, none fail, 274 match. Every bin the first histogram named
is now closed; the 27 refused programs' next stops are listed in
ISSUES-17.

## Stage 44 — abbreviated combined relation conditions **S** — DONE 2026-08-31

The last nucleus item ISSUES-10 named. `parse_simple` remembers the
last relation's subject, operator and NOT; an `AND`/`OR` followed by a
relational operator and an object, or by an object alone that is not a
condition-name and is not itself followed by an operator, builds the
relation on that subject (6.5.3), and `parse_not` above it supplies
the negation, so the abbreviations fall into the ordinary precedence
and parentheses. The relational-operator parse is factored out
(`parse_relop`). NC225A then wanted a condition-name alone as an
EVALUATE subject (`ALSO IT-IS-81`, answered by `WHEN ... ALSO TRUE`).
free/abbrcond, oracle agreeing; NC205A, NC211A, NC225A match, and so
does RL's last refused program, which makes RL complete; the suite
(a full run, the earlier running tallies had drifted by one) 280 of
303 compile, 6348 of 6390 pass, none fail, 277 match.
Ruling recorded this stage: the 85 text over GnuCOBOL -- what the
text forbids (a non-integer numeric MOVEd to an alphanumeric item,
three NC programs) stays refused.

## Stage 45 — INITIALIZE as the text has it **S** — DONE 2026-08-31

REPLACING was the ask; NC223A's 41 failures on the first cut were
the rest of 6.16. The template image (init_one, VALUEs ignored) is
now copied in runs around the bytes the text leaves alone --
elementary FILLERs, index items, REDEFINES items and their
subordinates, every occurrence (`init_mask`) -- and the edited items
then get a MOVE: ZERO to numeric-edited, SPACES through the edit to
alphanumeric-edited, which took a fix to emit_move (a figurative into
an alphanumeric-edited item was a fill; it is a move through the
edit). REPLACING touches only the named categories (GR2): the walk
(`init_replace_walk`) emits a MOVE per elementary item of the
category, tables unrolled over every occurrence. free/initrep, oracle
agreeing; NC223A matches; the suite 281 of 303 compile, 6442 of 6484
pass, none fail, 278 match.

## Stage 46 — ACCEPT FROM DATE, DAY, TIME, DAY-OF-WEEK **S** — DONE 2026-08-31

The runtime builds the text's integer (YYMMDD, YYDDD, HHMMSShh with
the hundredths from the clock, 1 Monday to 7 Sunday) and moves it to
the item by the MOVE rules through a numeric DISPLAY descriptor of
that width. free/acceptdate checks shapes rather than values (month
and day ranges, the year against FUNCTION CURRENT-DATE, an hour under
24, a weekday in 1..7), so the oracle can agree on any day; NC214M
matches; the suite 282 of 303 compile, 6443 of 6485 pass, none fail,
279 match.

## Stage 47 — SEARCH ... END without AT **S** — DONE 2026-08-31

"SEARCH needs at least one WHEN" was the parser reading `END` (AT
omitted) as nothing and then finding no WHEN behind the AT END
statements it had not consumed. `[AT] END` now. free/searchend
(SEARCH ALL on a two-key table and a serial SEARCH, the index shown
through a numeric item since DISPLAY of an index item is the
implementor's), oracle agreeing; NC237A matches; the suite 283 of 303
compile, 6456 of 6498 pass, none fail, 280 match.

## Stage 48 — a group as the STRING receiver **S** — DONE 2026-08-31

The receiver check refused every group; the text forbids only an
edited or JUSTIFIED item (6.24.2), and a group is alphanumeric.
free/strgroup (a group filled across its subordinates with a
POINTER, then the overflow), oracle agreeing; NC217A (82 tests)
matches; the suite 284 of 303 compile, 6538 of 6580 pass, none fail,
281 match.

## Stage 49 — qualified subscripts to 64 deep **S** — DONE 2026-08-31

The subscript parser took qualifiers but kept eight and dropped the
rest silently, so NC246A's eighteen-deep subscript came out
ambiguous. 64 now, an error past it, and a missing word after OF/IN
is an error rather than a stride. free/subqual (a ten-deep qualified
subscript, the same name in two records, an adjusted one), oracle
agreeing; NC246A (49 tests) matches; the suite 285 of 303 compile,
6587 of 6629 pass, none fail, 282 match.

## Stage 50 — a signed expression as a condition operand **S** — DONE 2026-08-31

`IF NINE * 9 - 7 * SEVEN NOT EQUAL - (SEVEN * 7) + 9 * NINE`: the
object begins with a unary minus, and parse_cond_operand routed to
the expression parser only after an operand or a parenthesis. A
leading sign is an expression too; and after AND/OR it is an
expression rather than the abbreviated relational operator of Stage
44 (only =, < and > start one). free/unaryexpr, oracle agreeing;
NC250A (115 tests) matches; the suite 286 of 303 compile, 6702 of
6744 pass, none fail, 283 match.

## Stage 51 — 64 operands **S** — DONE 2026-08-31

MAXOPS was 16; NC106A and NC176A add and subtract 21 operands. 64
now, the runtime's numeric stack 96 deep. free/manyops (21 items
added and subtracted, 30 literals GIVING), oracle agreeing; NC106A
(126 tests) and NC176A (124) match; the suite 288 of 303 compile,
6952 of 6994 pass, none fail, 285 match.

## Stage 52 — a doubled quote split across a continuation **S** — DONE 2026-08-31

The source reader decides whether a continued line ended inside a
literal by counting its quotes; NC215A's alphabet literal ends line
49 with a quote at column 72 and continues `-    ""9K...`, the two
quotes being the halves of an embedded doubled quote. When the
balance count says closed but the last character at column 72 is a
quote and the continuation line begins with the same one, the literal
is still open and the continuation's quote is the required marker;
the join then reads `""` as one embedded quote. tests/fixed/litcont
(built to column 72 by a script), the oracle agreeing on the output;
NC215A matches; the suite 289 of 303 compile, 6959 of 7001 pass, none
fail, 286 match.

## Stage 53 — the MOVE rules the NIST cases settled **M** — DONE 2026-08-31

The user reversed the text-first ruling: where the 85 text and the
NIST cases differ, the cases win. The refusal of a non-integer
numeric item MOVEd to an alphanumeric one came out, and the three
programs then showed four more MOVE rules: a group sending item is a
plain alphanumeric move whatever the receiver (emit_move now decides
that first, before the edited-receiver path); a numeric-edited
sending item is alphanumeric to an alphanumeric receiver (cob_move's
"numeric" test for that branch is COB_NUM alone); an alphanumeric of
more digits than the numeric receiver holds keeps the rightmost (28
digits into 9(10)); a P item's digits expand its P positions to
zeros (`num_to_digits`); and a floating picture with no 9 shows
spaces for zero, point and currency included (cobedit). free/numalnum
(the oracle refuses the MOVE outright, noted in oracles.md); NC105A,
NC114M and NC124A match, and an ST program that had stopped on the
same MOVE; the harness's bad/nonint-to-alnum test, which encoded the
refusal, is gone; the suite 293 of 303 compile, 7289 of 7335 pass,
none fail, 290 match.

## Stage 54 — SYMBOLIC CHARACTERS **S** — DONE 2026-08-31

The clause's names join the figurative constants: `is_figurative` and
`fig_byte` consult a per-unit table of name and byte (ordinal - 1),
so VALUE, MOVE, ALL, comparisons, STRING and INSPECT take them where
they take SPACE or ZERO. free/symchar, oracle agreeing; NC401M's
clause compiles, and the program (compile-only) then stops at ALTER,
where NC303M stops too; the suite unchanged at 293 of 303 compile,
7289 of 7335 pass, none fail, 290 match.

## Stage 55 — the obsolete elements: MEMORY SIZE, ALTER, STOP literal **S** — DONE 2026-08-31

NC302M's OBJECT-COMPUTER carries MEMORY SIZE 64000 CHARACTERS, which
the paragraph reader stopped at (a number); it skips it. The program
then ALTERs, as NC303M and NC401M do: the ALTER names are gathered in
the paragraph prescan, a GO TO in such a paragraph (its only
statement, possibly bare) jumps through a cell `.Lalt<unit>_<para>`
laid out with the unit's data and initialised to the GO TO's own
target, and the ALTER statement stores the new target there. STOP
literal displays the literal and goes on, there being no operator.
free/alter (PERFORM ... THRU ranges, as the NIST programs write it --
a performed paragraph that GO TOs out never returns), oracle agreeing
under -std=cobol85; NC302M and NC303M run, NC401M compiles; the NC
module compiles 95 of 95; the suite 296 of 303 compile, 7290 of 7336
pass, none fail, 293 match.

## Stage 56 — the last seven; the suite compiles 303 of 303 **M** — DONE 2026-08-31

SQ101M's `WRITE ... BEFORE ADVANCING ZERO` had collided with the PAGE
sentinel (-1) and written a form feed; ZERO lines is its own value
(-2): the record goes out without its newline and the file remembers
(`nl_pending`), the next write supplying it -- the lines GnuCOBOL's
file has. SQ111A's `CODE-SET IS` is accepted for a STANDARD-1/NATIVE
alphabet. SQ207M qualifies a record by its file (`PRINT-REC IN
PRINT-FILE`): sym_lookup's outermost qualifier may be the FD's
file-name. SQ303M/SQ401M `OPEN INPUT ... REVERSED`: bit 8 of the mode
to cob_open, which seeks to the end of a fixed-length sequential file,
and cob_read then delivers the record before the position (GnuCOBOL
ignores REVERSED and reads forward -- the text is followed,
oracles.md). ST139A/ST140A `SORT ... [COLLATING] SEQUENCE [IS]
alphabet`: a non-native alphabet's rank table is emitted
(`.Lalph<unit>_<i>`) and handed to cob_sort_begin, which sets
cob_collating for the merge and puts the program's back. free/sqst
(GnuCOBOL's forward-reading answer beside ours); the whole suite: 303
of 303 compile, 7314 of 7425 pass, none fail, 300 match -- the three
others being the obsolete-element programs that have no tests and
run.

## Stage 57 — the first performance pass, driven by batch.sh **M** — DONE 2026-08-30

majesty's batch ran in 1.83 s and the user asked whether the compiler
could take a share of it. The profile (docs/performance.md, method
and table) said the compiler was nowhere in it: 88% of a step was
libc's `fgetc` reading the file a byte at a time through `fread` and a
per-call `bytes / size` -- an unsigned divide the runtime performed as
a 32-round shift-subtract loop although the ISA divides in hardware.
Fixed from the bottom: `fgetc` takes the buffered byte itself and
`fread`/`fwrite` skip the divide for `size == 1`; `__udivsi3`,
`__divsi3`, `__umodsi3` on the hardware `div`/`rem` (both operands
under 2^31 in one instruction, the two big cases a line each; the
same routine in stage08's builtins64.s); `__udivdi3` with a 32-bit
divisor in two hardware steps (Hacker's Delight divlu). Then libcob:
digits nine at a time in a 32-bit word, two a step through a pairs
table, powers of ten as compile-time constants, the line-sequential
READ through the runtime's own buffer and memchr (cob_file grew
rbuf/rpos/rlen; the compiler emits the three words), the de-edit path
out of cob_get_num's frame, packed bytes straight from the digit
pairs. gl036: 2.83 G instructions to 0.30 G; the batch: 0.64 s, 2.6 G
instructions over all its steps, every report byte-identical; the NIST
suite, the harness and the regression suite (two new division-edge
tests) unchanged. tools/utilities/s32-hotspots.py is the attribution
tool. What is left is the COBOL program's own work at ~5,000
instructions a record; the levers not taken are listed in
performance.md.

## Stage 58 — the Screen section as the user's RM notes have it **M** — DONE 2026-08-30

The one item in the backlog with a program behind it (ISSUES-13;
menu.cbl and taskdt.cbl, batch.sh --menu). The focus loop rewritten
(docs/screen.md "As built (Stage 58)"): the terminal's cursor
sequences folded into key codes with a lone Escape told apart by
term_kbhit; fields in declaration order with Enter/Tab/Down forward,
Up/Shift-Tab back, Enter on the last submitting, Escape abandoning;
text edited in place with the cursor keys, Backspace and Delete
closing the gap, SECURE echoing stars, REQUIRED and FULL refusing to
leave; numeric fields edited on the point -- integer digits shifting
in, `.` to the fraction, sign keys where the picture has a sign --
rendered through the slot's picture after every key and committed
with cob_put_num at the picture's scale; UNDERLINE and LOWLIGHT as
SGR 4 and 2 (the term service passes codes through), COBOL colour
numbers mapped to ANSI's; LINE PLUS / COLUMN PLUS, and a slot without
LINE or COLUMN following the previous one. cob_scr_field's pad short
now carries fg/bg. free/screen2 pins the stream (reviewed by hand;
screen's re-pinned for the Backspace that now closes the gap); the
harness is 81; menu.s32x walks MAIN, DAILY, DATE PAGE and back.

## Stage 59 — CRT STATUS **S** — DONE 2026-08-30

The first item off ISSUES-23, at the user's ask. `SPECIAL-NAMES. CRT
STATUS IS item.` (saved per unit like the collating name; the ACCEPT
emits cob_crt_status(item, desc) before cob_screen_accept). The
runtime reports the ending in GnuCOBOL's numbering -- 0 ordinary,
1001+n function key, 2001/2002 page keys, 2005 Escape -- into a
numeric item, GnuCOBOL's packed three-byte form, or four digits of
text; scr_key learned the F1-F12 and page-key sequences and keeps one
byte of pushback so a lone Escape ahead of typed text is told from a
sequence. Function and page keys end the ACCEPT with the fields
committed; Escape still abandons. free/screen3 pins Enter/F3/PgDn/
Escape; the harness is 82.

## Stage 60 — nested screen groups **S** — DONE 2026-08-30

The second item off ISSUES-23. The screen parser keeps a stack of the
enclosing entries: an entry with a name and no PICTURE or VALUE
pushes a group whose flags and colours compose over its parent's and
reach every child (input-only clauses reach only TO/USING fields),
and whose LINE/COLUMN anchor the first child. A named group becomes a
second cob_screen record pointing into the parent's slot table at
first * sizeof(cob_scr_field), so DISPLAY and ACCEPT of the group
paint and focus just that window -- the runtime is untouched.
free/screen4 pins two levels of nesting, inherited reverse+colour,
LINE PLUS on an inner group, ACCEPT of both windows with CRT STATUS
still reporting; the harness is 83.

## Stage 61 — subscripted and LINKAGE slot items; the screen module closed **S** — DONE 2026-08-30

The last of ISSUES-23. A slot's FROM/TO/USING reference is recorded
as tokens and resolved lazily (sfield_resolve) -- at Screen Section
parse time the OCCURS dimensions are not built yet, the same reason
Report Writer defers SOURCE to GENERATE. Literal subscripts fold into
the static address; a runtime subscript, LINKAGE or EXTERNAL item
flags the slot dynamic (kind bit 7): its image points at a .data
cell, and emit_screen_dyn_fill re-parses the reference and stores the
address before every cob_screen_display/accept of a window containing
it -- USING CELL(I) follows I. The runtime grew scr_item()/scr_kind()
and nothing else. Contained programs may own screens: the screen
table gained g_screen_base like the symbol table's, and the old
refusal fell out. free/screen5: a dynamic slot re-ACCEPTed under a
moved subscript, a static literal-subscript slot, and a contained
program's LINKAGE screen editing the caller's element in place; the
harness is 84.

## Stage 62 — Report Writer's expensive half; the module entire **L** — DONE 2026-08-30

ISSUES-11, at the user's ask, cobc370's slices 3-5 as the design
donor. CONTROL with FINAL; break sensing major-to-minor against
hidden clones; the GENERATE sequence (CF minor-to-break with the
control items swapped to their prior values -- SOURCE and USE BEFORE
REPORTING both see them; clone-redirect leaked the new value to USE
and GnuCOBOL's behaviour exposed it -- then CH break-to-minor, then
subtotals, then the detail); SUM counters as referable hidden items
with UPON, RESET ON, crossfooting and rolling at the summed counter's
reset level; TERMINATE as a most-major break then PF then RF; RH/RF
with Table 5's placement (rf_begin writes paper -- in this engine
LINE-COUNTER is the paper, the register/paper split designed out);
NEXT GROUP's three forms (CF-below-break ignored); GROUP INDICATE as
a report-block bitmask; USE BEFORE REPORTING + SUPPRESS through the
declaratives machinery; GENERATE report-name. Four GnuCOBOL 4
divergences documented in report-writer.md and oracles.md, the text
and cobc370's IKFCBL00-corroborated derivations winning. rptctl,
rptnext, rptuse (.oracle-expected each); harness 87; NIST and majesty
unchanged. Out by choice: CODE, REPORTS ARE.

## Stage 63 — the IF module: every 1989 intrinsic function **M** — DONE 2026-08-30

ISSUES-22, at the user's ask ("time to be greedy again").  The module
extracted from newcob.val with EXEC85 rebuilt in the oracle container
(the old host binary linked the retired GnuCOBOL).  32 functions
added to the 10 majesty had: the numeric family (MAX/MIN/ORD-MAX/
ORD-MIN/SUM/RANGE/MIDRANGE/MEAN/MEDIAN/VARIANCE/STANDARD-DEVIATION
exact on the stack where possible; MOD/REM/INTEGER/INTEGER-PART/
FACTORIAL exact i64; SQRT/LOG/LOG10/SIN/COS/TAN/ASIN/ACOS/ATAN/
ANNUITY/PRESENT-VALUE via libm doubles, which the DBT hot-swaps
native) through one runtime entry popping the numeric stack and
returning a sign plus 18 digits, scale 0 or 9; MAX/MIN over strings
(the winning argument); CHAR/ORD; REVERSE; NUMVAL/NUMVAL-C (detached
signs, spaced currency); WHEN-COMPILED as a compile-time literal;
RANDOM as PCG-XSH-RR-64/32 from ~/tinymux's svdrand.  Arguments are
expressions, nested functions, or a table's elements via the ALL
subscript (compile-time unrolled); a condition may be functions
alone.  Found and fixed on the way: a separator comma must detach a
following parenthesis (MAX(B, (C+1)/2)); and three latent stack
hazards under S9V9(17) operands -- cob_nmul overflow (sheds fraction
digits first), cob_ndiv minting scale 19 (capped 18, and at least
nine guard digits so TAN(1/180)'s argument survives), cob_npow
overflow on SQRT(10) ** 2 (pre-reduces).  **IF: 45 of 45 programs,
735 of 735 tests, 45 matching GnuCOBOL exactly.** free/intrinsics
(oracle agrees on every value) and free/fnall (GnuCOBOL refuses the
amendment's ALL subscript; documented).  The harness is 89.

## After v1 (not scheduled, each when a program asks)

The ranked, maintained form of this list is [../ISSUES.md](../ISSUES.md)
(2026-08-30); what follows is the list as written at v1.

- ~~Rest of majesty batch~~ (done, Stages 19-21)
- ~~In-program `SORT`~~ (done, Stages 19-21 and ISSUES-4a)
- ~~Nucleus Level 2 at full width~~ (done, Stages 26-56)
- The rest of the corpus rewrite: the pure-COBOL date-function family
  (`fielded_to_linear.cbl`, `linear_to_fielded.cbl`, `isvaliddate`,
  `isleapyear`, `floor-div`, `floor-divmod`, `holidays`' inner units)
  is still `FUNCTION-ID`; its invocations sit inside arithmetic and
  conditions and need hoisted temporaries, and nothing on `batch.sh`
  reaches them -- do it when `jerm`/`exgltrans` are compiled, with a
  test plan of its own
- ~~Report Writer `CONTROL`/`SUM`~~ (done, Stage 62)
- ~~Alternate keys~~ (done, ISSUES-12)
- dBase-compatible writer filter
- ~~CCVS-85 NC/SQ/IC as a pass/fail suite~~ (done, all nine modules: ccvs-run.sh)
- `rs.c`, `csvgen.c`, `crc.c` on SLOW-32, when a program calls them

Queued 2026-08-30, after the suite and the screen (ISSUES-22, 23):

- The IF module -- X3.23a-1989's 44 intrinsic functions, 45 CCVS-85
  programs already in the tree, 34 functions to add; the one
  85-adjacent standard with an answer key
- Screen leftovers: `CRT STATUS` and exception keys, nested screen
  groups, subscripted/LINKAGE slot items

Sort-Merge as a module, Debug, Communication stay out.

## What not to start a stage with

- Copying `fortran/src/hir_*.h`
- Copying `cobc370.c`
- A dBase-openable file as the first indexed test
- Report Writer `SUM` before gl030 prints
- SSA
