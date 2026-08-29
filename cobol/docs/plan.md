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

## Stage 3 — decimal library + edited MOVE **L**

- Canonical numeric, `COMP-3`, DISPLAY numeric, scale, `ROUNDED`,
  `ON SIZE ERROR`
- Edited pictures, de-edit; `----,---,--9.99` with **negative**
  values (see [report-writer.md](report-writer.md))
- `COMPUTE`, `ADD … GIVING`

Done: cobc370's `arith`/`compute`/`edtest`-shaped tests, rewritten
as 85, green against GnuCOBOL. Majesty amounts (`pic s9(9)v99
comp-3`) round-trip.

## Stage 4 — line sequential I-O **M**

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

## Stage 5 — indexed I-O **L**

- Default path in [indexed.md](indexed.md)
- `gl039` compiles and runs
- Random `READ … KEY IS` by `desc-id`, `INVALID KEY`, `END-READ`

Done: gl039 then a tiny reader prints descriptions by id.

## Stage 6 — subprograms and the C bridge **M**

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

## Stage 7 — Report Writer, cheap half **L**

- [report-writer.md](report-writer.md) v1 subset
- Print files are line sequential

Done: **gl022, gl023, gl030** match `reports_cobol/`. This is the
first product claim. GnuCOBOL can still be on the rest of the
majesty path.

## Stage 8 — SCREEN SECTION, first screen **M**

- [screen.md](screen.md) against `term.h`
- `CBL_GET_SCR_SIZE`; `USAGE BINARY-CHAR [UNSIGNED]`
- The `PIC X(6)` → `PIC S9(3)V99 COMP-5` `MOVE` usescreen makes

Done: `usescreen.cbl` runs.

## Stage 9 — the menu, and what it drags in **L**

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

## Stage 10 — sequential V / RDW **M**

- [framing.md](framing.md)
- Round-trip with tapemgr

Done: a V file this compiler writes is read by tapemgr; a V file
tapemgr writes (or cobc370's `vrec` output, ASCII-translated if
needed) is read back.

## Stage 11 — v1 close

- Intrinsics at exactly the v1 set: `LOWER-CASE`, `UPPER-CASE`,
  `LENGTH`, `CURRENT-DATE`. Nothing else is referenced by a gate.
- `batch.sh`'s `run_cobol` switched from `cobcrun -M MAJESTY` to the
  emulator for the retired programs (see
  [majesty-corpus.md](majesty-corpus.md))
- Document remaining majesty programs as Stage 12+
- cobc370 still untouched

**v1 is done.** GnuCOBOL is retired for the journal and both charts
of accounts, and for the two screen programs.

## After v1 (not scheduled, each when a program asks)

- `ACCEPT … FROM ARGUMENT-VALUE / ARGUMENT-NUMBER` — the first thing
  needed, because gl024 (the journal pipeline's first step) takes
  `YYYYMM` that way, as do eight other programs
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
