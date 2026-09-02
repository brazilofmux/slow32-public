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

State on 2026-08-31: harness **91/91**; CCVS-85 **348 of 348 compile**,
**8049 of 8160** tests pass with none failing (ISSUES-17); majesty
`batch.sh` runs every COBOL report step on SLOW-32 with all twelve
reports byte-identical; **every program in `~/majesty/src/cobol`
compiles, 56 of 56**. The sweep that measures the last number is one
line, run from `~/majesty`:

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
unused and were removed on the majesty side. The other SPECIAL-NAMES
clauses all landed later: switches (Stage 23), `DECIMAL-POINT IS
COMMA` (Stage 27, a token post-pass), `CURRENCY SIGN` (Stage 43),
`SYMBOLIC CHARACTERS` (Stage 54), `CRT STATUS` (Stage 59).

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

### 11. ~~Report Writer: `CONTROL` breaks and `SUM`~~ — RESOLVED 2026-08-30 (Stage 62)
The module is entire now: CONTROL/CH/CF with prior values by swap,
SUM with UPON and RESET and rolling, RH/RF, NEXT GROUP, GROUP
INDICATE, USE BEFORE REPORTING with SUPPRESS, summary GENERATE
(docs/report-writer.md "The expensive half"; four GnuCOBOL
divergences documented, the text winning). Out by choice: CODE,
REPORTS ARE. The original entry, for the record:
The expensive half of the module. majesty's reports compute their
totals in the Procedure Division, so the page engine
(`docs/report-writer.md`) is enough for all twelve. Stage 7 chose
this deliberately; do not start it without a report that needs it.
Stage 32 (2026-08-31) widened the page half to the NIST RW module --
clauses in any order, PAGE FOOTING, FOOTING, LINE-COUNTER/PAGE-COUNTER
as items -- 6 of 6 match GnuCOBOL; CONTROL/SUM/NEXT GROUP/GROUP
INDICATE/RH/RF/USE BEFORE REPORTING remain here.

### 22. ~~The IF module: X3.23a-1989 intrinsic functions~~ — RESOLVED 2026-08-30 (Stage 63)
All 42 functions of the amendment are in; the IF module extracted and
run: **45 of 45 programs, 735 of 735 tests, 45 matching GnuCOBOL's
tally exactly** (IF401M-403M compile-only, as report.pl has them).
The numeric family goes through the numeric stack into one runtime
entry returning a signed 18-digit string (scale 0 for the integer
class, 9 for the fractional; doubles via libm where the math needs
them -- the DBT runs those natively); MAX/MIN over strings return the
winning argument; NUMVAL/NUMVAL-C parse the 85 shapes with detached
signs and currency; CHAR/ORD are inverses; WHEN-COMPILED is a
compile-time literal; RANDOM is PCG-XSH-RR-64/32 (tinymux's
generator).  The suite smoked out three latent stack-arithmetic
hazards (S9V9(17) operands): cob_nmul overflow, cob_ndiv minting
scale 19, cob_npow overflowing on SQRT(10) ** 2 -- all hardened.  A
separator comma now detaches a following parenthesis (MAX(B, (C+1)/2)
is not a subscript).  GnuCOBOL 4 refuses the ALL subscript the
amendment defines (free/fnall documents it).  The original entry:
The 1989 amendment is the one COBOL-85-adjacent standard, and its
test module sits in the same CCVS-85 suite, unextracted
(`ccvs-run.sh` prints `IF not extracted` every run): 45 programs,
IF101A-IF142A and IF401M-IF403M, over 44 functions. The compiler
knows the ten majesty asked for -- `CURRENT-DATE`, `UPPER-CASE`,
`LOWER-CASE`, `LENGTH`, `INTEGER-OF-DATE`, `DATE-OF-INTEGER`,
`INTEGER-OF-DAY`, `DAY-OF-INTEGER`, `RANDOM`, `SUM`. The other 34:
`NUMVAL`, `NUMVAL-C`, `MOD`, `REM`, `INTEGER`, `INTEGER-PART`, `MAX`,
`MIN`, `ORD-MAX`, `ORD-MIN`, `CHAR`, `ORD`, `REVERSE`, `WHEN-COMPILED`,
`MEAN`, `MEDIAN`, `MIDRANGE`, `RANGE`, `VARIANCE`,
`STANDARD-DEVIATION`, `ANNUITY`, `PRESENT-VALUE`, `SQRT`, `LOG`,
`LOG10`, `EXP`, `SIN`, `COS`, `TAN`, `ASIN`, `ACOS`, `ATAN`,
`FACTORIAL`, and the `ALL` argument / table arguments the statistics
take. Same oracle, same runner, same authority order (NIST cases,
then the text, then GnuCOBOL where it agrees). The transcendentals
are the interesting part on this platform: the cases pin results to
fixed decimal places, so it is libcob's precision (soft-float or
hardware FP) against an answer key. Nothing majesty runs needs any of
it -- queued, not scheduled.

### 23. ~~Screen section leftovers~~ — CLOSED 2026-08-30 (Stages 59-61)
~~`CRT STATUS` and the exception keys~~ — Stage 59, 2026-08-30
(screen.md; GnuCOBOL's numbering, F1-F12/PgUp/PgDn end the ACCEPT
with the fields kept, Escape abandons; free/screen3).
~~Nested screen groups~~ — Stage 60, 2026-08-30 (inherited look, the
group's LINE/COLUMN anchoring its first child, DISPLAY/ACCEPT of a
named group as a window into the parent's slots; free/screen4).
~~Subscripted or LINKAGE items in a slot~~ — Stage 61, 2026-08-30
(the reference recorded as tokens and re-parsed at each ACCEPT/
DISPLAY into a .data cell the slot points at; literal subscripts stay
static; contained programs may own screens; free/screen5). What
remains by choice: `BLINK`, `BELL`, `ERASE EOL/EOS` accepted without
effect, and reference modification in a slot refused. The screen
module is done.

### 12. ~~Indexed: `ALTERNATE RECORD KEY`, `DUPLICATES`~~ — RESOLVED 2026-08-31
One sorted table per key (docs/indexed.md "Alternate keys"); key of
reference; partial-key START; 02/22 per the text. free/altkey; NIST IX
28 of 29 programs and 405 of 406 tests matching GnuCOBOL.

### 13. ~~Screen: the user's eventual target~~ — RESOLVED 2026-08-30 (Stage 58)
Recorded in `docs/screen.md` from RM COBOL / Micro Focus experience:
TAB order across fields with Enter as submit, numeric fields anchored
on the decimal point, `AUTO`, `SECURE`, reverse video or underline.
All of it is in the focus loop now (screen.md, "As built (Stage 58)"):
cursor keys, in-place text editing, numeric entry on the point through
the slot's picture, SECURE/REQUIRED/FULL, underline and colours, LINE
PLUS / COLUMN PLUS. free/screen2; menu.s32x walks its four screens.
Left: nested screen groups, subscripted/LINKAGE slot items, CRT
STATUS -- each when a program asks.

### 14. ~~OCCURS DEPENDING ON is laid out at its maximum~~ — a group MOVE lands 2026-08-30
Still laid out at the maximum, which is the 1985 receiving length
when the DEPENDING ON item is outside the group. A MOVE *of* such a
group now sends its current length (`cob_move_odo`; since Stage 33
the table may sit at any depth, as long as nothing follows it in the
group -- variable-location items are refused by name; free/odonest).
free/odomove; GnuCOBOL's receiving length is the current one --
documented divergence (oracles.md).

### 24. Statement cost — the generic runtime call where a copy would do — GitHub #27, mostly RESOLVED 2026-09-01
Section B had no performance items until the majesty side profiled
`batch.sh` and found the COBOL report path spending ~70% of one join
step in `cob_move` and `cob_cmp`. Three compile-time fixes landed, plus a
fourth the first one exposed:

- **A `MOVE` between byte-identical descriptors is a `memcpy`.** It was
  going through `cob_move`, which for numeric-to-numeric decodes with a
  digit loop and re-encodes with a divide loop -- 646 instructions to
  copy the ten bytes of a `PIC 9(10)` (see the table below). This is also a
  *conformance* fix, which is the surprise: GnuCOBOL passes the bytes
  through, and the round trip did not. A numeric item holding bytes
  `cob_put_num` would never write -- spaces in a field nothing filled
  in, an `0xF` sign nibble on a COMP-3 record from a foreign system, a
  COMP past its picture -- arrived rewritten. free/identmove.
- **A four-byte unsigned item may use the hot compare.** It was barred
  because no signed SLT orders a value with the top bit set, which is
  true of arithmetic and not of comparison: the SLTU family orders the
  whole range, and COBOL unsigned items are never negative. `PERFORM
  UNTIL ws-i > 56164` was building a descriptor for the literal and
  calling `cob_cmp`, ~440 instructions for one SGTU. free/hotarith.
- **Truncation to the picture uses the range it already knows.** `ADD 1
  TO` an item already inside its picture can pass the limit once, so it
  wraps with a compare and a subtract instead of `REM` -- a divide, and
  it sat in the hottest loop COBOL has. Where the value cannot reach the
  limit at all the truncation goes entirely, and with it the sign fixup.

Measured on kagura (see `bench/`, which is self-contained -- `bgen`
writes its own synthetic input, so nothing private is needed):

    per statement, guest instructions   before   after
    PERFORM VARYING iteration              485      32
    numeric MOVE, PIC 9(10) -> 9(10)       646      75
    PIC X MOVE (average of five)           116      93

(`after` is the shipped COPY_INLINE_MAX of 8.  At 16 the two MOVE rows are
20 and 82 instead, because a ten-byte field then goes inline -- but that
costs the DBT 41% on MOVE-heavy code, which is the trade the next paragraph
is about.  Quoting the 16 numbers as the result would be quoting a build
nobody runs.)

`majesty`'s twelve reports stay byte-identical, which is the gate; its
`batch.sh` went 2.62s -> 2.03s there, and the COBOL programs in it
1.99s -> 1.49s summed. Note that batch's wall time is not all COBOL:
much of it is the shell, the sorts, and one process per step.

Not done, and the reason section 24 is only *mostly* resolved: a
relation against a *literal* still builds a descriptor whenever either
side is not a hot integer, and the group `MOVE` + `WRITE` path (974
instructions per 75-byte record) has not been looked at. Neither is
scheduled; the corpus stopped asking.

- **A copy of a size the compiler knows goes inline, up to
  `COPY_INLINE_MAX` bytes,** instead of calling `memcpy`. See the
  threshold note below: it is 8, and which number it is matters.

**Why the threshold is 8, and why the corpus decides it.** The engines
disagree: `slow32-dbt` recognises the `memcpy` entry point by name and
substitutes a native stub, so a call there is nearly free, while the
interpreters execute every instruction of it. `bench/sweep.sh` on
`bench/b3big`, both hosts (arm64 DBT re-timed at four decimals):

    COPY_INLINE_MAX      0      8     16     24     40
    kagura fast (s)  15.61  12.76   8.75   8.51   6.33
    kagura dbt  (s)  0.230  0.220  0.310  0.310  0.450
    arm64  fast (s)   7.96   6.59   4.05   4.05   2.83
    arm64  dbt  (s)  0.0725 0.0781 0.3215 0.3225 0.4710

Same shape on both -- interpreters want it big, the DBT falls off a cliff
between 8 and 16, and the cliff is in the same place -- so one constant is
right rather than one per host. But note that b3big's DBT column does
**not** agree with itself across hosts at the 0/8 boundary: kagura
prefers 8, arm64 prefers 0 by ~8%. A tiebreak of "the DBT is what runs
the corpus" reads off b3big as 0 on arm64.

It shouldn't, and the corpus itself is why. Every threshold, majesty's
s32x rebuilt from each, reports byte-identical at all of them (arm64):

    COPY_INLINE_MAX      0            8           16           40
    guest instructions   2099450533   2046857172  1983245824   1963697060
    batch.sh (s)         0.40         0.40        0.42         0.43

8 uses 2.5% *fewer* guest instructions than 0 and ties it on wall time,
so 8 is right on arm64 too -- for the opposite reason b3big gives. And 16
is a 5% wall regression on the corpus, not the 4.4x b3big predicts.

The useful part is the inversion: from 8 upward guest instructions go
**down** while wall time goes **up**. The inline copy really is fewer
instructions; it just costs more than the DBT's native stub. So
instruction count is the wrong metric for tuning *this constant*, though
it is the right one for the rest of section 24. b3big amplifies that
until it flips the 0/8 answer, because a MOVE-only loop is nothing but
the thing being measured.

So: re-sweep with `bench/sweep.sh` if you like, but **decide on the
corpus**, and never on one engine. Otherwise the next person re-sweeps on
b3big on an arm64 box and moves the constant to 0 with good evidence and
a worse result.

### 26. Comparison cost — a DISPLAY numeric relation was a runtime call — GitHub #29, shape (1) landed 2026-09-02
#27 widened the inline compare to four-byte unsigned BINARY items and
stopped there, because `opnd_hot_cmp` requires *both* operands to be
`is_hot_int` and that bails on `U_DISPLAY`. So every relation touching
a DISPLAY numeric still built two descriptors and called `cob_cmp`.
Measured on the corpus afterwards, `cob_cmp` was called 2,365,352 times
against `cob_move`'s 271,445 -- comparisons outnumber generic moves 8.7
to 1, and #27 had optimised the moves.

Shape (1): a relation between two items of one byte-identical **unsigned
DISPLAY** descriptor, no editing and no P, lowers to `memcmp`. Same
length, digits and scale, so the points line up and byte order is
algebraic order.

    per compare, guest instructions       536 -> 73
    b5big, slow32-fast                  34.2s -> 3.8s
    b5big, slow32-dbt                   2.83s -> 0.32s
    corpus batch.sh (kagura)            2.03s -> 1.79s, reports identical

**Signed is excluded as a correctness condition, not a precaution.** An
overpunched last byte does not order like its digit: `'001B'` against
`'0012'`, GnuCOBOL and gcobol both say *less*, `memcmp` says *greater*
(`B` is 0x42, `2` is 0x32). Two independent implementations against the
byte order. The separate-sign forms and BLANK WHEN ZERO are out for the
same reason -- a sign character or a space sorting against a digit.

**What it is not: a conformance fix.** An earlier draft said so on
GnuCOBOL's evidence alone and was wrong. The text compares the
*algebraic value* of numeric operands whatever their usage, and for
canonical fields of one descriptor byte order and algebraic order
coincide -- so the two readings cannot disagree on any datum the
standard admits. They part only on a numeric item holding non-digits,
which the text does not define, and there three compilers give three
answers ('  12' against '0012'): GnuCOBOL compares bytes, gcobol 15.3.0
and s32-cobc-before decode. We moved onto GnuCOBOL's answer -- which is
the implementation ranked *last* for grounds, so note that the byte
compare is taken on its own merits (exact on every defined value, ~7x
cheaper) and the agreement is a side effect. free/cmpbytes pins it so a
later change is visible.

Do not reason from this to #27's identical-descriptor MOVE or back. Same
surface shape, different footing: a MOVE between identical descriptors
is byte movement and all three implementations agree on exactly the
bytes that split them here.

Not done: shapes (2) and (3) of the issue -- an unsigned DISPLAY integer
against a binary or literal operand (gl024's and gl036's hot path), and
ADD/SUBTRACT on DISPLAY and packed at 896 instructions.

### 25. An unsigned COMP-5 value past 2^31 is stored as its magnitude — GitHub #28
Found writing free/hotarith, and older than the tests: a NOTRUNC field
is a plain unsigned word, but both store paths treat a value with the
top bit set as negative and store `|v|`. GnuCOBOL keeps the value.
Nothing in the corpus reaches it -- every unsigned item built from a
picture holds at most 999999999 -- which is also why free/hotarith
cannot prove its unsigned compare is *necessary* rather than merely
equivalent. Filed rather than fixed: it wants its own CCVS-85 pass.

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
- A `MOVE` of a non-integer numeric item to an alphanumeric receiver is
  **accepted**, moving the digits as stored with the sign and the point
  unrepresented; GnuCOBOL calls it "invalid MOVE" and refuses it in
  every dialect. The 1985 text forbids it too, but the NIST cases
  (NC105A, NC114M, NC124A) require it, and the cases won -- Stage 53,
  the user's ruling of 2026-08-31 (086ee808). free/numalnum, which
  therefore has no oracle and whose `.expected` is the cases' answer.
  This entry was missing until 2026-09-02: `docs/dialect.md` still
  claimed we refused it, and the harness reporting an oracle refusal as
  a pass (1a4ce2d1) is what kept the contradiction from surfacing.

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
Follow-up (GitHub #22, 2026-08-30): both assemblers (`slow32asm` and
the selfhost `s32-as`) now do the same relaxation at the right layer,
so this pass is belt-and-suspenders; it can be retired whenever
touching the compiler next, since the assembler catches whatever it
would have.

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
condition). The real gate ran too: every compiled program's own
PASS/FAIL lines are the tally above (the IF module joined at Stage
63, extracted with EXEC85 rebuilt in the oracle container).

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
`~/s32x/cc.s32x` (and kagura's copy) was rebuilt with both fixes the
same evening; probed 2026-08-30 through the kit's own cc/as/ld.

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
