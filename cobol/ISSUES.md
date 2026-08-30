# COBOL 85 front (`cobol/`) — open items and post-mortems

The in-tree engineering log for `s32-cobc` and `libcob`, kept next
to the code (CLAUDE.md: cite an entry as **`cobol ISSUES-N`**, never a
bare `#N`). The stage history is [docs/plan.md](docs/plan.md); the
measured corpus table is [docs/majesty-corpus.md](docs/majesty-corpus.md);
this file is what is *open*, ranked, plus what was closed and why.
Nothing here is scheduled: the front is app-driven, and an item moves
when a program asks for it.

**Operating mode (ruled 2026-08-30, with the corpus at 56 of 56):** the
split between `~/majesty` and `cobol/` is clean. majesty cleans up and
consumes -- it builds with `s32-cobol`, runs under `slow32-dbt`, and
files what it needs as GitHub issues. `cobol/` gets serious about the
*language* (CCVS-85, Nucleus level 2, the rest of §B) and validates
that nothing has been broken: the harness with its GnuCOBOL oracle on
every change, and majesty's batch (twelve reports byte-identical) as
the regression gate before a push. Corpus programs are not rewritten
from this side; they are majesty's.

State on 2026-08-31: harness 49/49; CCVS-85 210 of 303 compile, and of those 4062 of 4097 tests pass with none failing, 207 programs matching GnuCOBOL (ISSUES-17) with the GnuCOBOL oracle agreeing
on every program that has one; majesty `batch.sh` runs every COBOL
report step on SLOW-32 with all twelve reports byte-identical; **every
program in `~/majesty/src/cobol` compiles, 56 of 56** (2026-08-30 evening). The sweep that
measures the last number is one line, run from `~/majesty`:

    for f in src/cobol/*.cbl; do ~/slow-32/cobol/out/s32-cobc -free -m -I src/copy -o /dev/null $f; done

## A. The corpus — no refusals left; the items, as they were closed

### 1. ~~RELATIVE I-O (3 programs: crglentry, ldglentry, exglentry)~~ — RESOLVED 2026-08-30
Stage 19. Slots of `4 + recsize` framed with the mode-V RDW (zero =
empty), which also carries glentry's variable-length records; the
six verbs under random, dynamic and sequential access, statuses and
positioning measured against GnuCOBOL (docs/indexed.md "As built").
crglentry and exglentry run on SLOW-32 with GnuCOBOL's output;
ldglentry now stops at its `SD` (ISSUES-4). The on-disk bytes differ
from GnuCOBOL's 8-byte native length -- documented, and no program
outside COBOL reads these files.

### 2. ~~The legacy `FUNCTION-ID` date family (7 units + 2 callers)~~ — RESOLVED 2026-08-30 on the majesty side
Kagura converted the family to subprograms rather than retiring it
(majesty e69e98b: FUNCTION-ID → PROGRAM-ID, RETURNING → a trailing
USING argument, every invocation a CALL, temporaries hoisted where an
invocation sat inside an expression), verified byte-identical under
GnuCOBOL over jerm's 400,001 lines. All thirteen units and both
callers compile here now. The C `du_*` path stays the deployed one.

### 3. ~~`SPECIAL-NAMES` — `CLASS name IS '0' THROUGH '9'` (damm)~~ — RESOLVED 2026-08-30
Stage 16: a 256-entry membership table per class in the literal pool,
per program unit; the test beside `NUMERIC` in `parse_simple`;
`cob_class_user` in the runtime. damm then wanted console `ACCEPT`
(one line of stdin, moved as text) and `LENGTH OF`; both landed, and
damm's output is byte-identical to GnuCOBOL's over seven inputs
including the check-digit fixtures majesty's tests use. gl008's declarations were
unused and were removed on the majesty side. Other SPECIAL-NAMES
clauses (`CURRENCY SIGN`, `SYMBOLIC`) stay refused by name until a
program asks; switches landed in Stage 23 and `DECIMAL-POINT IS
COMMA` in Stage 27 (a token post-pass, not the scanner).

### 4. ~~`SD` and file `SORT` (glacpost; ldglentry)~~ — RESOLVED 2026-08-30
Stage 21. The SD is a `cob_file` of organization SORT; a SORT
statement's records live in memory (RELEASE appends, a merge sort on
an index array orders them -- stable, so WITH DUPLICATES IN ORDER
costs nothing -- RETURN hands them back); USING reads through the
input file's own READ and GIVING writes through the output file's own
WRITE, so the two keep their organizations. Keys are items of the SD
record, ascending or descending, up to sixteen. tests/free/sortfile
covers USING/GIVING, INPUT/OUTPUT PROCEDURE with RELEASE and RETURN
... INTO, two keys in opposite directions and DUPLICATES IN ORDER;
GnuCOBOL agrees. glacpost (stdout, `sorted.tmp`, the new master) and
crglentry → ldglentry → exglentry are byte-identical to GnuCOBOL.
Not done: MERGE, COLLATING SEQUENCE, a spill to disk (the corpus
sorts thousands of records, not millions).

### 4a. ~~Table `SORT` (gl008, dist01) — GitHub #10~~ — RESOLVED 2026-08-30 by rewrite
Ruling: rewritten in majesty to COBOL 85 -- insertion sorts through a
holding element (stable; a 2002 table `SORT` leaves equal keys
unspecified). Under GnuCOBOL the old and new gl008 print twelve
receipts byte-identically; on SLOW-32 the same twelve match GnuCOBOL.
The same commit rewrote a subscripted subscript (`cat-tax(ws-id(i))`,
also 2002) and dist01's `OCCURS UNBOUNDED` and 21-digit item
(ISSUES-5). `ROUNDED MODE NEAREST-EVEN` was first made plain
`ROUNDED`, which moves every exact half-cent the other way; the
user's call was to keep half-to-even, so majesty 7f2d3ce / 06f5cc1
write it out in 1985 arithmetic (gl008 `072-round-half-to-even`;
dist01 from DIVIDE's exact REMAINDER), swept against GnuCOBOL's
nearest-even over 40,001 values and 13,824 splits. The MODE phrase
itself stays refused (`bad/rounded-mode`).

### 5. ~~A numeric item of more than 18 digits (dist01)~~ — RESOLVED 2026-08-30 by rewrite
`s9(18)v999` became `s9(15)v999` in majesty; the compiler keeps the
standard's limit and names it.

### 6. ~~`FUNCTION INTEGER-OF-DATE` / `DATE-OF-INTEGER` (jerm2)~~ — RESOLVED 2026-08-30
Stage 18: the four calendar functions of the 1989 addendum
(`INTEGER-OF-DATE`, `DATE-OF-INTEGER`, `DAY-OF-INTEGER`,
`INTEGER-OF-DAY`), integer 1 = 1601-01-01, invalid input gives 0. A
result rides the intrinsic plumbing as numeric DISPLAY digits (ten for
a day count, eight for a date, seven for a day-of-year -- the widths
GnuCOBOL shows when the value is DISPLAYed directly). free/datefn agrees
with GnuCOBOL; jerm2 -- majesty's 400,000-day cross-check of the C
`du_*` routines against these functions -- compiles, runs on SLOW-32
in 0.4 s under the DBT, and reports no disagreement on either engine.

### 7. ~~`USAGE BINARY-INT UNSIGNED` (testcrc)~~ — RESOLVED 2026-08-30 by rewrite
majesty: `PIC 9(9) COMP-5` (four bytes, the C seam's unsigned 32-bit),
the hex literal in decimal, `CBL_NOT` as `4294967295 - item`. Prints
zlib's CRC-32 of 'A' on both engines.

### 8. ~~`XML` / `JSON` verbs (usexml, usejson)~~ — RESOLVED 2026-08-30 by deletion
GnuCOBOL extension probes, 2002 by construction; removed from majesty
(3f342ed). Nothing in the corpus refuses now.

### 9. gl015, gl016 — a report field without a PICTURE
Both are retired programs, not in majesty's build. Not counted.
(The Stage 12+ corpus table used to list them under a subscripted
`SOURCE`; that refusal now belongs to live gl008 — ISSUES-19.)

### 19. ~~Subscripted `SOURCE` in a Report Writer field (gl008) — GitHub #9~~ — RESOLVED 2026-08-30
The field keeps the token position of its SOURCE reference and
`parse_ref` reads it at GENERATE, where every other reference is
parsed -- so subscripts, `OF` qualification and reference
modification all come with it (`nm(i)(1:3)` included). Test
free/rptsub prints straight out of an ODO table, GnuCOBOL agreeing.
gl008's next stop was `ROUNDED MODE` (2002; majesty writes half-to-even
out in 85, see ISSUES-4a), then its table `SORT` (ISSUES-4a / GitHub #10).

## B. Language — known gaps no program has asked for

### 10. Nucleus level 2 remainder
~~`MOVE/ADD/SUBTRACT CORRESPONDING`~~ (Stage 36), ~~abbreviated
combined relation conditions (`a > b and < c`)~~ (Stage 44), ~~nested programs~~
(Stage 29), ~~`REPLACE`, `COPY ... REPLACING`~~ (Stage 26, 2026-08-31),
~~the full `INSPECT` (BEFORE/AFTER INITIAL, CONVERTING, the one-pass
rule)~~ (Stage 35). Each is a diagnostic today, never silence.

### 11. Report Writer: `CONTROL` breaks and `SUM`
The expensive half of the module. majesty's reports compute their
totals in the Procedure Division, so the page engine
(`docs/report-writer.md`) is enough for all twelve. Stage 7 chose
this deliberately; do not start it without a report that needs it.
Stage 32 (2026-08-31) widened the page half to the NIST RW module --
clauses in any order, PAGE FOOTING, FOOTING, LINE-COUNTER/PAGE-COUNTER
as items -- 6 of 6 match GnuCOBOL; CONTROL/SUM/NEXT GROUP/GROUP
INDICATE/RH/RF/USE BEFORE REPORTING remain here.

### 12. ~~Indexed: `ALTERNATE RECORD KEY`, `DUPLICATES`~~ — RESOLVED 2026-08-31
One sorted table per key (docs/indexed.md "Alternate keys"); key of
reference; partial-key START; 02/22 per the text. free/altkey; NIST IX
28 of 29 programs and 405 of 406 tests matching GnuCOBOL.

### 13. Screen: the user's eventual target
Recorded in `docs/screen.md` from RM COBOL / Micro Focus experience:
TAB order across fields with Enter as submit, numeric fields anchored
on the decimal point, `AUTO` (auto-tab when full), `SECURE` (`*`
masking), and fields drawn in reverse video or underline. As built
(Stage 8) the screen paints and accepts field by field. `UNDERLINE`
also needs the term service to grow an attribute (`open-questions.md`).

### 14. ~~OCCURS DEPENDING ON is laid out at its maximum~~ — a group MOVE lands 2026-08-30
Still laid out at the maximum, which is the 1985 receiving length
when the DEPENDING ON item is outside the group. A MOVE *of* such a
group now sends its current length (`cob_move_odo`; since Stage 33
the table may sit at any depth, as long as nothing follows it in the
group -- variable-location items are refused by name; free/odonest).
free/odomove; GnuCOBOL's receiving length is the current one --
documented divergence (oracles.md).

## C. Documented divergences from GnuCOBOL (not bugs — the text wins)

Kept in `docs/oracles.md` and `docs/dialect.md`, each with a
`.oracle-expected` beside the test that shows it:

- REWRITE of an absent key → status 23 (GnuCOBOL 21).
- WRITE of a record longer than `VARYING ... TO` → 44 (GnuCOBOL
  clamps and reports 00).
- Sequential mode V on disk carries the IBM RDW, length inclusive of
  the header (GnuCOBOL: a private length word, exclusive); measured
  by a `tapemgr` round trip on every V file the tests write.
- An over-long LINE SEQUENTIAL record → 04, the rest of the line
  dropped (GnuCOBOL 4 splits it into two records with 06).
- `CALL` by name folds case (`'twice'` finds `TWICE`), as the static
  link does; GnuCOBOL's dynamic lookup is case-sensitive.

## D. Harness and infrastructure

### 15. ~~The oracle vanished with the host GnuCOBOL~~ — RESOLVED 2026-08-30
GnuCOBOL was uninstalled from every host, and `run-tests.sh` chose
its oracle with `command -v cobc`, so the suite went on passing with
no oracle and said nothing. Now: `gnucobol:4.0-builder` (cobc) and
`gnucobol:4.0-runtime` (the built program) under podman or docker,
repo bind-mounted at its own path; the work directory moved under
`cobol/out/` because a podman machine on macOS cannot mount `/tmp`;
the last line names the oracle, or says `NO ORACLE`.

### 16. ~~`RESTORE.JCL` committed by accident~~ — RESOLVED 2026-08-30
`tapemgr create` writes a `RESTORE.JCL` into its working directory
(a real feature: the MVS job that restores the tape). The Stage 10
harness ran it from `cobol/`, and the file went in with `34d5a81e`.
Removed; the harness now runs tapemgr inside its work directory.

### 20. ~~A conditional branch past ±4096 bytes~~ — RESOLVED 2026-08-30
gl008's `100-allocation-reports` was the first PERFORM body longer
than a bcond can reach; the assembler refused the program ("Branch
offset out of range ... 4424 bytes away"). The compiler now keeps its
assembly in memory and relaxes: every instruction line it writes is
one 4-byte instruction (`li`/`la` are already spelled out), so .text
positions are exact, and a branch that cannot reach becomes its
inverse over a `jal` (±1 MB), iterated to a fixed point. gl008 needs
four; tests/free/farbranch two. Found only because the corpus's
biggest program finally compiled -- the sweep's value again.

### 21. MOVE from a numeric-edited item holding malformed text
Feeding ldglentry a lines file of the wrong schema put `000066C00000`
into `pic 9(9)v99+` and moved it to a packed item: GnuCOBOL made
`-6600000.04` of it, we made `+6600000.00`. Garbage in; the 1985 text
says the sending item's content must be a valid edited value. Left
open only so the difference is on record; not worth matching.

### 17. CCVS-85 as a histogram — RUNNING since 2026-08-30 (Stage 22)
`tests/ccvs-histogram.sh` over the extracted modules in
`~/gnucobol-svn/tests/cobol85` (X-cards already substituted there).
4 → 202 of 303 in one day; `tests/ccvs-run.sh` then runs and scores
them by their own reports: **303 of 303 compile; 7314 of 7425 tests pass, none fail, 300
programs match GnuCOBOL's tally exactly** (the three others are the
obsolete-element programs with no tests, which run) (Stage 23; alternate keys
made IX 29 of 29, LINAGE the SQ page tests, COPY REPLACING/REPLACE
made SM 12 of 13, DECIMAL-POINT IS COMMA 13 of 13; the IC bin was
the runner not building `lib/`, then `CALL identifier`/`ON
EXCEPTION`/`CANCEL` -- IC 16 of 25). The
remaining bins, largest first, each a
work item: ~~`ALTERNATE RECORD KEY`~~ (done), ~~`LINAGE`~~ (done),
~~`COPY ... REPLACING`~~ (done), ~~`CALL identifier`~~ (done), ~~an ODO
table nested below a direct child~~ (done), ~~`UNSTRING`~~ (done; NC218A
and NC247A match, the ODO group's current length in every operand use),
~~`INSPECT ... BEFORE/AFTER INITIAL`~~ (done, with the one-pass rule and
CONVERTING: all four match), ~~`MOVE/ADD CORRESPONDING`~~ (done: five
programs match), ~~nested programs~~ (done), ~~`EXTERNAL`~~ (done), ~~`BY CONTENT`~~
(done: **IC 25 of 25**), ~~Report Writer clauses~~ (done: **RW 6 of
6**, ISSUES-11 keeps CONTROL/SUM), ~~alphanumeric-edited pictures
with A/9 mixed and `;` in a picture~~ (done; those programs go on to a
non-integer numeric MOVEd to an alphanumeric item (2), ~~`REMAINDER`
with a ROUNDED quotient~~ (done, with SIZE ERROR and an edited
receiver: NC203A and NC251A match), ~~RENAMES~~ (done: NC252A matches),
~~`USAGE` on a group~~ (done), and NC114M's `0` statement), ~~`USAGE INDEX` on a
group~~ (done: NC131A, NC135A match), ~~more than three `VARYING ...
AFTER` levels~~ (done, with WITH TEST AFTER across levels: NC201A,
NC233A, NC243A match), ~~a multi-character `CLASS` literal~~ (done, with
switches from the environment and SET groups: NC174A, NC254A match),
~~`CURRENCY SIGN`~~ (done, with BLANK WHEN ZERO on a plain numeric item
and procedure-names of digits: NC107A, NC108M match). **No program is refused any more (2026-08-31, Stage 56): the suite
compiles 303 of 303.** What the last ones stopped on: ~~a non-integer numeric
MOVEd to an alphanumeric item (NC105A, NC114M, NC124A)~~ (done, Stage
53: the user reversed the text-first ruling -- the NIST cases are the
standard's executable form and win where they and the text differ),  ~~"too many operands" (NC106A, NC176A)~~ (done: 64 operands, Stage 51),
~~abbreviated combined relations (NC205A, NC211A, NC225A)~~ (done, Stage 44),
~~ACCEPT FROM DATE/DAY/TIME (NC214M)~~ (done, Stage 46), ~~a literal continued in a way the
reader refuses (NC215A)~~ (done: a doubled quote split at column 72, Stage 52), ~~a STRING receiver that is a group (NC217A)~~ (done, Stage 48),
~~INITIALIZE REPLACING (NC223A)~~ (done, Stage 45), ~~SEARCH with no WHEN (NC237A)~~ (done: `END` without `AT`, Stage 47), ~~an
ambiguous subscript name (NC246A)~~ (done: 64 qualifiers, Stage 49), ~~`-` as a data-name start (NC250A)~~ (done: a signed expression operand, Stage 50),
~~NC302M's ENVIRONMENT DIVISION (MEMORY SIZE), ALTER (NC303M, NC401M),
STOP literal~~ (done, Stage 55: NC compiles 95 of 95), ~~SYMBOLIC CHARACTERS
(NC401M)~~ (done, Stage 54; NC401M then wants ALTER, as NC303M does); ~~ADVANCING ZERO (SQ101M), CODE-SET (SQ111A), a record qualified by its
file (SQ207M), OPEN REVERSED (SQ303M, SQ401M), SORT [COLLATING] SEQUENCE
(ST139A, ST140A)~~ (done, Stage 56; RL's last program was the abbreviated
condition)., `MOVE/ADD CORRESPONDING` (4; ISSUES-10), `BY CONTENT` (2),
more than three `VARYING ... AFTER` levels (2), clauses on an 01
report group (2), `EXTERNAL` (2), a multi-character `CLASS` literal
(2), `CURRENCY SIGN` (1). Then the real gate: run each compiled
program and read its own PASS/FAIL lines (the IF module wants
`make IF` there first).

### 18. Building on a host without LLVM
`cctool.sh` (b96c4aff, Kagura) falls back to the self-hosted stage08
`cc.s32x` under the emulator when `$LLVM_BIN/clang` is absent. That
route exposed a stage08 parser gap — a block-scope declarator list
ending at a brace initializer — filed as GitHub #8, worked around in
`libcob.c` (957b5a29), and fixed in the parser on 2026-08-30
(`parse_local_declarator`; stage08 `tests/test_phase32.c`; the unsplit
`libcob.c` compiles again). The same route then found GitHub #11 --
a file-scope `long long` array initializer repeating its low word,
which made every COBOL division return 0 through `pow10tab` -- fixed
the same day (selfhost ISSUES-62). The fallback is now exercised by the
whole harness: with `LLVM_BIN=/nonexistent` (libcob and the C bridge
through `cc.s32x`) it runs 46/46 with the oracle agreeing. The kit
`~/s32x/cc.s32x` carries both fixes only once the kit is rebuilt.

## E. Closed, with the lesson

- **Out-of-line `PERFORM` swallowed the enclosing `END-PERFORM`**
  (sweep, Stage 13): the paragraph form must not `accept` a scope
  terminator that belongs to an outer inline PERFORM.
- **Alphanumeric → numeric MOVE parses the text as decimal**, measured
  against GnuCOBOL (usescreen printed 42.25 for 50.00 before).
- **Report Writer page rules** were measured, not read: the fit test
  counts printing lines; a body line past `LAST DETAIL` spills to a
  new page at `FIRST DETAIL`, with the heading rendered inline by the
  compiler; `TERMINATE` only pads. An earlier "TERMINATE starts a
  page" rule was wrong and is gone.
- **`has_odo` looks at children only** — one occurrence of the ODO
  item itself still moves.
- **Refmod vs subscript**: `x(1:3)` and `x(1)` share the paren; look
  ahead for `:` before parsing a subscript list.
- **Static buffer in `link_name()`** clobbered the main wrapper's
  entry name; copy into a local.
- **2x file statuses are the invalid-key condition**, not errors
  (`file_result` returns 1, the statement's `INVALID KEY` branch runs).
- **tapemgr dropped `binary`/`codepage` on extract** — a majesty bug
  the V round trip found, fixed there (249292b). None found in
  cobc370 yet; when one is, it is filed in `~/cobc370`.
