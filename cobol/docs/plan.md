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
form majesty rewrites to plain `ROUNDED`, then its table `SORT`
(GitHub #10, a product ruling).

## The batch, whole — 2026-08-29

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
