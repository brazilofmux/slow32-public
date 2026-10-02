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
Not done then: MERGE, a spill to disk (the corpus sorts thousands of
records, not millions). Done 2026-09-04: the sort runs on one normalized
key per record (memcmp), spills budgeted runs beside the SD and merges
them k ways (`libcob/xsort.h`; S32_SORT_MEMORY, S32_SORT_FAN), and MERGE
merges its USING files as presorted runs instead of sorting their
concatenation. tests/free/sortkeys, sortspill, mergefile; bench/bsort.
COLLATING SEQUENCE landed earlier (the alphabet's ranks go into the key).

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

**Shape (2), landed 2026-09-02.** An unsigned DISPLAY integer of at most
nine digits -- below 10^9, so it fits a word -- decodes in line on the
compare path and compares in a register. This is the MIXED case, `PIC
9(8)` against a binary or a literal, which shape (1) cannot reach by
construction, and the Macbook's per-program numbers had already
identified it: after shape (1), gl025 went 1.00x -> 1.55x while gl024
stayed at 39,504,167 instructions to the byte, because gl024 and gl036
compare `PIC 9(8)` DISPLAY against a `usage signed-int`.

    per compare, guest instructions   498 -> 39
    b6big, slow32-dbt               2.865s -> 0.141s

The decode masks with `& 15` rather than subtracting `'0'`, which is what
`cob_get_num` does -- digits mask to themselves, a space masks to 0
(cob_get_num's space-is-zero rule), anything else to its low nibble (its
fallback). So unlike shapes (1) and (3) this one has no undefined-input
divergence at all. It was free; it would have been careless not to take.

**Shape (3), landed 2026-09-02.** The operand half was worth 2% and the
receiver half was worth 12x, which is worth knowing in that order: `ADD 1
TO` a `PIC 9(7)` DISPLAY cost 644 instructions because `cob_top_addto`
does a `cob_get_num` *and* a `cob_put_num` on the receiver. With
`emit_load_int` decoding a DISPLAY integer and `emit_store_int` encoding
one, the existing hot machinery works unchanged and it costs 54.
PERFORM VARYING and SET UP/DOWN BY on a DISPLAY index reach it too.

Two semantics had to survive, and free/hotdisp pins them: a DISPLAY item
is *exactly* its digits, with none of the slack that lets
`emit_trunc_bounded` skip a binary field, so 999 + 1 in a `PIC 9(3)` is
000; and an unsigned receiver takes the magnitude, so 3 - 5 is 002. All
nine lines agree with GnuCOBOL 4.0 *and* gcobol 15.3.0.

**Still out of reach**, and it is the issue's headline number: `ADD PIC
9(9)V99 TO PIC S9(11)V99` at 896. Eleven digits with a scale is not a
word; it needs 64-bit inline arithmetic. gl036 does one per detail line,
so that is what would move gl036, and nothing here does.

**And the lever for it is not in `cobol/` at all — GitHub #30.** Profiling
that ADD on a host without LLVM put ~1300 of its 2532 instructions in one
64-bit divide. Fixing it *here* (a `udiv_small` in libcob, 0f83d5c2) was a
47% win on the self-hosted build and a **60% regression** on the LLVM one,
because LLVM strength-reduces the constant divisor and never calls a
helper; reverted in e29fb799. `libcob.s32o` is gitignored and every host
builds its own from this shared source, so a source-level workaround for
one compiler's codegen is a trap for whoever measures next.

The real defect: `selfhost/stage08/builtins64.s`'s `__udivmoddi3` is still
a 64-round shift-subtract loop, while `runtime/builtins.c`'s `__udivdi3`
has had the 32-bit-divisor fast path since the work in
`docs/performance.md`. `builtins64.s32o` links before `libs32.s32a` and
first definition wins, so hosts without clang get the slow copy with the
fast one sitting in the archive behind it. Same COBOL `MOVE`: 2195
instructions self-hosted against 718 under LLVM.

**Lesson, and it is the cheap one: ask which compiler builds the thing you
are optimising before you optimise around its output.**

**Re-measured 2026-09-02, after GitHub #30 landed on both legs** (b774e514
put the sr-seeded loop into `runtime/builtins.c` too). The bench set,
guest instructions per statement on the MacBook, loop floor subtracted;
the LLVM leg links `libs32.s32a`, the self-hosted leg (`LLVM_BIN` hidden
so `cctool.sh` takes the kit) links the tree's `builtins64.s`:

    statement                                   llvm   self-hosted
    compare, identical unsigned DISPLAY (b5)      73       73
    compare, DISPLAY vs binary/literal (b6)       48       48
    ADD 1 TO PIC 9(7) DISPLAY (b8)                65       65
    ADD DISPLAY ints TO S9(11)V99 (b7, x3)       548     1020
    ADD 9(9)V99 TO S9(11)V99 (c3, #30's bench)   893     1119
    ADD 9(9)V99 TO S9(11)V99 (b9a, the headline) 930     1250
    MOVE S9(13)V99 TO S9(11)V99 (c9)             715     1149
    loop floor, per iteration (b0)                40       40

What #30 bought: the self-hosted scaled ADD went from 2532 to 1119 and
the scaled MOVE from 2195 to 1149, and the two legs are now within
1.25-1.6x of each other where they were 2.8-3x apart. What it could
not buy: the LLVM leg's 893 is unchanged, because LLVM never called the
helper -- that number was always the cost of `cob_top_addto` itself
(two `cob_get_num`, `cmp_scaled`-style 64-bit alignment, `cob_put_num_x`
with its digit loop), and it is what a "shape (4)" would be measured
against. The three inline shapes hold on both legs to the instruction,
as they should: they do not touch libcob.

**Counted 2026-09-02, the batch's runtime calls after #27/#29/#30** (libcob
instrumented locally, not committed; MacBook, LLVM leg, slow32-dbt for the
counts and slow32-fast for the totals). The batch is now **1.52 billion
guest instructions** (2.05 before #27).

    calls across the batch     cob_cmp 1,066,377   cob_move 259,662
                               cob_top_addto 208,889   cob_top_store 14,420
                               cob_nmul 11,998 (all gl036)   cob_nsub 2,760
    cob_top_addto by program   gl034 57,243   gl036 56,190   gl038 55,258
                               gl040 39,766   (the rest under 500)
    instructions by program    gl034 235M  gl036 228M  gl035 214M  gl038 173M
                               gl030 169M  gl037 136M  gl042 115M  gl040 78M

So the scaled ADD is **~190M of the 1.52B, about 12%**, and a shape (4)
that took it from ~900 to the DISPLAY-integer shape's ~65 would buy at
most ~11% of the batch, in four programs. The residual `cob_cmp` is the
larger pool: 1.07M calls (2.37M before #29) at the 300-540 the issue
measured is 20-35% of the batch, and *which* shapes those are -- signed
DISPLAY, unequal scales, PIC X of unequal length -- is the question to
answer before choosing between the two. Neither is proposed here.

**The residual `cob_cmp`, classified 2026-09-02** (cob_cmp keyed locally
by both descriptors' cat/usage/size/digits/scale/flags; not committed):

    1,062,732  99.7%  PIC X against PIC X, one byte each, identical descriptors
        1,918   0.2%  S9(11)V99 COMP-3 against the same
          937   0.1%  S9(11)V99 COMP-3 against a one-digit literal
          483         PIC XX against PIC XX
          303         9(11)V99 COMP-3 against a one-digit literal
    1,066,377  total

It is one shape, and it is not a numeric one: the flag test.
`ws-accounts-eof-flag PIC X VALUE 'N'` ... `PERFORM UNTIL ... = 'Y'`,
`act-crdb`, `act-class`, `d-lin-type`, every program, 58k-190k each
(gl030 190k, gl037 160k, gl038 148k, gl034/gl036/gl035 ~114k). At the 84
instructions #29 measured for `PIC X = PIC X` that is ~90M of the
batch's 1.52B, about 6% -- *less* than the scaled ADD's ~12%, because
the per-call cost is a quarter of a numeric compare's. The inline form
is a byte load and an SEQ. The numeric shapes #29 left are, to the
batch, gone: 3,158 calls in all.

So the two levers, sized: the scaled ADD (shape 4, ~12%, four
programs, needs 64-bit inline scaled arithmetic) and the one-byte
alphanumeric compare (~6%, every program, needs almost nothing).
Neither is proposed here.

**The one-byte alphanumeric compare, landed 2026-09-02.** `cmp_is_onebyte`:
a one-byte alphanumeric or alphabetic item against another, a
one-character literal, `ALL 'x'` or a figurative, under the native
collating sequence, is a `ldbu` and one SEQ/SNE/SLTU. No padding (both
sides are one byte) and byte value is collating order, so it is exact
by construction rather than by choice, unlike shape (1). Bars: a
PROGRAM COLLATING SEQUENCE (the runtime compares through its table),
groups, 88s, reference modification, a numeric class on either side,
both sides literal. free/cmp1byte pins every operator, the ordering
('a' above 'Z', space below '0'), the figuratives, a subscripted
element, and the two shapes that must stay on the runtime path and pad
(`PIC XX` against `'Y'`, `PIC X` against `'Y '`).

    per compare, guest instructions (bx)      91 -> 14
    batch, guest instructions        1,520,311,792 -> 1,439,926,783  (-5.3%)

The counted estimate was ~90M; the batch gave back 80M. cobol/tests
99/99 with the oracle agreeing on the new case, CCVS-85 unchanged,
majesty's reports identical. What remains of #29 is the scaled ADD.

**The scaled ADD, landed 2026-09-02 -- and it was not the shape the
benches measured.** Keyed by descriptor, all 208,889 `cob_top_addto`
calls were a **COMP-3** receiver of eleven digits at scale 2 with a
same-scale operand: DISPLAY 9(9)V99 (42%), the same COMP-3 picture
signed (25%) or unsigned (26%). `ws-debits`, `ws-total-debits`,
`yt-debits(i)`, `ws-detail-balance`. Same scale, so nothing to align --
and that is what makes it a word-sized problem: each item is read into
two limbs of base 10^9 and a sign, the limbs add or subtract as
sign-magnitude with one carry, one REM brings the result inside the
picture, and it is written back as nibbles or digits. No call, no
descriptor, no 64-bit arithmetic; eighteen digits is the ceiling.
`sym_dec_ok` / `emit_dec_load` / `emit_dec_add` / `emit_dec_store`,
for ADD x TO r and SUBTRACT x FROM r with one operand, DISPLAY (trailing
overpunch or unsigned) or COMP-3 both sides, ROUNDED admitted (nothing
to round), SIZE ERROR, GIVING and literals left generic.

    per ADD, the batch's shape (bp)          919 -> 176
    batch, guest instructions   1,439,926,783 -> 1,292,295,055  (-10.3%)
    gl038 173M -> 122M, gl034 226M -> 188M, gl036 220M -> 181M

free/decadd pins signs crossing zero both ways, a zero result's sign,
the carry between the limbs, truncation past the picture (the 85
magnitude rule for an unsigned receiver, kept), an even digit count's
pad nibble, the DISPLAY receiver's overpunch, eighteen digits, a
subscripted receiver, two receivers, an item added to itself, ROUNDED.
Thirty lines, GnuCOBOL identical on all of them. cobol/tests 100/100,
CCVS-85 unchanged, majesty's reports identical.

Today's three inline shapes together: the batch 1.520 G -> 1.292 G,
-15%. What the runtime still does per batch: 260k `cob_move`, 12k
`cob_top_store` with a literal (gl036), and the file I/O. #29's levers
are spent.

### 25. ~~An unsigned COMP-5 value past 2^31 is stored as its magnitude~~ — GitHub #28, RESOLVED 2026-09-02
Found writing free/hotarith, and older than the tests: a NOTRUNC field
is a plain unsigned word, but a value with the top bit set was treated
as negative and stored as `|v|`. `ADD 2000000000 TO` a `PIC 9(9) COMP-5`
holding 2000000000 gave 294967296.

**One site, not two.** The filing named `cob_put_num_x` as well, and it
was never wrong: `cob_get_num` reads an unsigned binary item unsigned,
so the generic path holds the true 64-bit value and 4000000000 is
simply 4000000000 there (`ADD v5 TO u5` with two such items was correct
throughout). The defect was the hot path alone, where the sum is a
*word* and 0xEE6B2800 is 4000000000 and -294967296 at once; its sign
fixup chose the latter. The fix is `ref_hot_store`: a four-byte
unsigned NOTRUNC receiver stays hot only for an ADD of non-negative
operands, where the result cannot be negative and the word is stored
as it is; a SUBTRACT, SET DOWN, or a possibly-negative operand takes
the generic path, where the rule below is decidable. Narrower COMP-5
items keep a genuine sign in a word and are unchanged.

**The rule, and where GnuCOBOL leaves it.** The filing's "taken modulo
2^32 -- stored as-is" and the 85 text's "an unsigned receiver takes the
magnitude" coincide on the headline case and part on a negative result.
Measured (free/notrunc, both compilers): on every MOVE both take the
magnitude, as the text says. On arithmetic GnuCOBOL 4.0 takes the
magnitude for `SUBTRACT s9 FROM u5 GIVING u5` and the value modulo 2^32
for `SUBTRACT s9 FROM u5` -- the same operands, the same result, two
answers, split by the presence of GIVING (and `COMPUTE u5 = s9 - 10`
one way, `COMPUTE u5 = u5 - 10` the other). That is a native-binary
fast path leaking, not a semantics, so the text's rule stands
everywhere and the seven lines where GnuCOBOL wraps are in
`notrunc.oracle-expected` (docs/oracles.md). Consistent with the
COBOL 85 authority order: GnuCOBOL is oracle where it agrees with
the text, and here it does not even agree with itself.

free/notrunc also carries the compare free/hotarith could not: two
values past 2^31 ordered by SLTU, and PERFORM VARYING stepping across
it. Validated: cobol/tests with the oracle, CCVS-85 unchanged
(8049 of 8160, 0 fail), majesty's corpus green.

### 25. Indexed files: the key file is a B+tree, not a sorted array (2026-09-04)

Memory was the reason. SLOW-32 has no sbrk: a program's heap is what it
was linked with, and the indexed key tables were arrays loaded whole at
OPEN, one per key, growing with the file. An out-of-order WRITE shifted
the array's tail, so a shuffled load was quadratic; DELETE left its slot
dead for good; CLOSE rewrote the whole key file. Fine for gl039's 3,113
descriptions, a wall at a few hundred thousand.

Now (`libcob/btree.h`, docs/indexed.md "As built"): one `<data>.key` of
4K pages, one B+tree per key, fixed-width entries of (key, arrival
number, slot) ordered by key then arrival -- every entry distinct, so
duplicates need no special case and come back in arrival order; leaves
linked both ways; a bitmap of live slots so a DELETEd slot is reused;
pages through a pinned LRU cache (S32_INDEX_CACHE, else a sixteenth of
the heap, 16..256 pages) over lseek/read/write. The cursor is the (key,
arrival) of the next record to deliver, a lower bound, so a WRITE,
REWRITE or DELETE between READ NEXTs needs none of the position fixups
the arrays did. The prime entry carries each alternate's arrival number,
so DELETE and a REWRITE that moves an alternate remove exactly instead of
walking the duplicate run -- the first cut walked it and was still
quadratic on a duplicates-heavy key. No key compression (it raises leaf
fan-out and rarely lowers the height), no rebalancing beyond freeing a
page that empties. An S32KEY01/02 file is converted in place on OPEN.

Program-visible behaviour is unchanged: tests 105/105 (idxbig new: 3,000
records, two alternates, shuffled load, random reads, START/READ NEXT
scans, DELETE/REWRITE churn, slot reuse, under a 16-page cache), NIST IX
438/439 with 29 programs matching GnuCOBOL exactly -- identical to the
array build's tally -- and majesty's gate PASS. bench/bidx, shuffled
load plus churn, guest instructions:

    records     arrays        tree
      3,000   1,025 M       228 M
     30,000  92,070 M     2,925 M

The tree is host-testable (tests/bt_test.c, Gate 1b: random inserts,
removals by slot and exact, seeks and scans against a model, six shapes,
under the sanitizers when built by hand).

### 26. Where majesty's batch spent its time, and what moved it (2026-09-05)

The batch on host sorts ran in 0.36s; with every sort on s32sort and every
join on the B+tree it ran in 0.88s. Profiled to 0.26s. What was true:

- **Process startup is not the cost.** An empty program starts in under
  10ms under any engine; the interpreter is 25x slower overall. Timers
  built on perl or python add 10-20ms per step and inflate every profile;
  a shell trace with timestamps does not.
- **The ring is cheap in bulk, dear per call.** 4,000 round trips move a
  55,000-line file in under 10ms; 50,000 of them cost 100ms. The indexed
  writer seeked (and so flushed) before every record: 24,584 records were
  49,000 syscalls. `slot_write` now tracks the stream position and the
  next slot is one buffered fwrite. Random reads went through a 1K stream
  buffer, two round trips each; an INPUT open now reads the data file
  into memory when it fits a quarter of the heap (`idx_cache_load`),
  write-through on WRITE/REWRITE.
- **s32sort's own read path cost more than its sort.** The automatic
  record-length pass read the input with getc: 4.47M calls for one file,
  4,000 instructions a record, found by llvm-cov counts on the host build
  after three wrong guesses. Blocks now. The engine was also linked with
  the 1M default heap and spilled every batch sort; 64M.
- **The legs were serialized by a dependency that no longer existed**
  (the balance leg's sorted files). Running the three at once was the
  single largest win, 0.45 -> 0.26.
- **Five sort steps folded into their consumers** as SD SORT USING with
  an OUTPUT PROCEDURE, on the same engine: fewer processes and file
  round trips on the critical path. The COBOL heap is 64M for that.

Left on the table: libcob's line-sequential read and write cost ~2,200
instructions a record and a keyed join ~5,400; the DBT runs both at
~8 BIPS. The critical path is now two legs of five programs each at
35-50ms; the next lever is that per-record cost, not structure.

**The keyed join, last (2026-09-05).** After the record cache the join
consumers (a transaction-line file READ by key against a 55k-record master,
in key order) still spent their time in the B+tree descent: root to leaf,
one binary search per level, for every lookup, though 98% of them land
within a step of the previous one. `bt_first_ge_near()` keeps the last
(page, index) per open file plus the tree's modification count; if nothing
changed it pins that leaf and tries the neighbours of the remembered index
before falling back to `bt_first_ge()`. Measured with `S32_IDX_STATS=1`:
55,268 lookups, 54,542 answered from the hint, 726 full descents. The two
join programs went 27 -> 19 ms and 27 -> 17 ms; the batch itself stays at
about 0.25 s because it is already at the process-startup floor.

The first version was wrong, and the stress test said so before anything
was committed: at the front of a leaf it accepted entry 0 as the answer
whenever it was >= the target, but an earlier leaf may hold entries between
the target and that one, so a target smaller than the leaf's first key must
either match it exactly or fall through to the descent. Symptom on real
data: the join wrote 35,409 of 55,268 records and reported the rest as not
found. `tests/bt_test.c` now cross-checks the hinted lookup against the
plain one on every seek; that check is what caught it (three shapes, all
mismatching), and it is the reason this shipped correct rather than fast.

### 43. ~~Reserved words are accepted as user-defined names~~ — RESOLVED 2026-09-27
**Resolved.** The compiler had no reserved-word table; keywords were
told apart by context, so a name like FD or RD went through wherever
the parser could read it. It now carries the 1985 list (GnuCOBOL's
`-std=cobol85`, 348 words) and refuses one wherever a program names a
data item, index, file, paragraph or section:
`'fd' is a reserved word and cannot name a data item`. Four
`tests/bad/rw-*` fixtures, one per kind of name.

The survey came first, because refusing blindly would break preserved
code: 74-era programs used words that 85 later reserved. Over majesty
(0), the Open Systems suite, CCVS-85 (0) and the tests, the only
reserved words used as names are `CLASS` (four payroll programs) and
`OTHER` (PAACEMP, and tests/fixed/rmother), both newly reserved in
1985, plus the harness's own `TAPE` in tests/warn/every-point, renamed.
`CLASS` and `OTHER` are accepted, with `TRUE`, `FALSE` and `ANY` that
91e6807f took with `OTHER`, as behavior point BP-N1, a new class N in
docs/behavior-points.md; `-warn-74` names them. SPECIAL-NAMES names
(class, alphabet, symbolic character, mnemonic) are checked too, since
the same day's follow-up (`tests/bad/rw-class`); the corpora use none.

The original entry:
Open, unscheduled. `s32-cobc` accepted `SELECT FD ...` and a record
named `RD`; GnuCOBOL `-std=cobol85` refuses both ("unexpected FD",
"unexpected RD"), as the 85 text requires: a reserved word is never a
user-defined word. Found while writing `tests/free/sortgive2`. Not yet
surveyed: whether the compiler has a reserved-word list at all, or
tells words apart by position. A program that is valid never meets
this, so it is a conformance gap and not a miscompile; it belongs in
Stage A (`docs/standards.md`) as one bounded piece, with a
`tests/bad/` fixture per word class.

### 44. A file written only `AFTER 1` had no line breaks; CCVS-85 surveyed (2026-09-27)
**Surveyed.** The 111 CCVS-85 tests that did not pass were not
failures: 16 are deleted by the suite's own configuration and 91 are
marked for visual inspection (89 of them the SQ module's `M`
programs), both exactly as GnuCOBOL scores them. The last four were
three programs GnuCOBOL scores with special rules in `report.pl` that
`tests/ccvs-run.sh` did not implement, and one of the three hid a real
bug.

**The bug.** A file with no ORGANIZATION clause that is written with
ADVANCING becomes a print file (records are lines). The test was the
advancing *counts*, and `AFTER 1` is zero newlines beyond the record's
own, so a file written only `AFTER 1` -- never `AFTER 2`, never
`AFTER PAGE` -- stayed plain sequential and got no line breaks at all.
NC113M's whole report came out as one line. Every other CCVS program
also writes a page heading, which is why only this one showed it; a
real program printing a list one line at a time would have met it.
Fixed: the phrase decides, not its count.

**The runner.** `report.pl` scores NC113M by its MARGIN TESTING lines
coming out in sequence (total from "n TESTS REQUIRE VISUAL
INSPECTION"), and NC121M and NC220M by each `*** INFORMATION ***` line
matching the line the program DISPLAYed on the console. The runner now
does the same, with one reading: those patterns want a space after the
matched word, and a line-sequential print file trims trailing spaces,
so end of line counts too.

Result: **348 of 348 programs match GnuCOBOL's tally exactly** (was
345), 8068 of 8175 pass with none failing; `tests/ccvs-baseline.txt`
updated. Stage A (docs/standards.md) is left with the 91 inspection
tests to be read against the text and the gaps the suite does not
test.

Found on the way, not a compiler matter: the Open Systems AR harness's
cash-flow paper differs from its pin because the invoice date defaults
to the run date and the three aging dates in its key script are fixed
(15 and 30 September, 31 October 2026). Pinned on 7 September, the
invoice aged into the first column; run on 27 September, into the
second. The same paper comes out of the 7 September compiler and the 7
September DBT. The harness masks dates as text but not their effect.
GnuCOBOL answers this with `COB_CURRENT_DATE`; libcob has no such
override yet.

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
- `COB_CURRENT_DATE` fixes the clock completely (ISSUES-45). GnuCOBOL
  4.0 fixes every date field the same way, but lets the real clock
  through elsewhere: the time of day under the date-only form, the
  hundredths under `YYYY/MM/DD hh:mm:ss`. Here the missing fields read
  zero and CURRENT-DATE's offset reads +0000, so a fixed run prints the
  same bytes every time. GnuCOBOL accepts and ignores a trailing offset
  or fraction; here anything but the two documented forms is fatal.
  free/fixclock shows the fields both agree on.
- `MOVE ALL "123"` to a `PIC 99V99` item gives 31.00, the 1985 text's
  own example (XVII-82, X3J4 interpretation B-23), and `ALL "12"` to
  `9(5)` gives 12121; GnuCOBOL gives 12.00 and 21212 (ISSUES-47,
  free/moveall).
- A print file is a line printer (ISSUES-46): an overprint is a carriage
  return, so the second record lies over the first; GnuCOBOL appends it
  to the line, which leaves it 120 columns to the right. A WRITE with no
  ADVANCING advances one line, as SQ101M requires; GnuCOBOL does not
  advance at all. GnuCOBOL starts a print file one line lower, before
  the first record; this runtime prints the first record on the first
  line (a ruling, not yet read against the text). free/printer shows
  all three, with GnuCOBOL's layout in printer.oracle-expected.

### 27. Run s32-cobc under AddressSanitizer, with its tables forced to grow
`lit_label` returned a pointer into `g_lit`, a table it reallocates, and
callers hold that pointer while building an Arg list. It produced
`lui r5, %hi()` -- a reference to no symbol, resolving to address 0 --
and cost a CCVS regression (NC122A) that took two machines a day to
find, because:

- it needs the table to cross a power of two *between* two `lit_label`
  calls, so it takes a program with ~80 literals and no small
  reproduction of the statement shows it;
- it is a use-after-free, so whether it shows at all depends on the
  allocator. macOS returned an empty string; glibc quietly returned the
  old bytes, and the same compiler on the same source was correct on one
  host and wrong on the other.

Neither the harness nor CCVS nor the corpus can be relied on to catch
the next one: the corpus stayed byte-identical throughout.

What catches it: **`regression/run-cobc-asan.sh`** (the Macbook's, cfc6c5ab)
-- s32-cobc built with `-fsanitize=address` and pointed at a real corpus.
It tests the *invariant* (no held pointer into a growable table) rather
than any one arrangement that exposes a violation, so it does not go
stale the way a pinned test would. A forced-realloc build is **not**
needed, which an earlier draft of this entry wrongly said: ASan poisons
the old block on every realloc that moves, so plain ASan is enough.

It is self-validating -- `COBC_SRC` aims the build at any revision:

    git show 18fcb42c:cobol/src/s32-cobc.c > /tmp/buggy.c
    COBC_SRC=/tmp/buggy.c regression/run-cobc-asan.sh NC   # must FAIL
    regression/run-cobc-asan.sh NC                         # must PASS

**A clean run is worth exactly what the corpus was.** Measured on kagura
2026-09-02: with the CCVS modules skipped, `cobol/tests` alone (83
compiles) does not reach lit_label's growth boundary, and the script
returns green *on the buggy compiler*. It now says so in its summary
rather than printing a green line. Run it where `newcob.val` is.

Which also means the `g_files[fd].name` shape -- the same
`static const char *` into a growable array -- is **unaudited, not
exonerated**. The Macbook's clean 453-compile run does not reach it;
nothing in either corpus opens enough files to grow `g_files` while a
name is held. Closing it needs a source that does, and the harness will
catch it the day one exists, but it will not invent one.

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

### 28. The Open Systems suite (~/open): the four small refusals — RESOLVED 2026-09-05

GitHub #34-#37, filed from the 1978-83 Open Systems RM/COBOL accounting
suite (228 programs). Each fix carries a corpus-shaped test in tests/fixed,
and GnuCOBOL agrees on all four.

- **#37 comment-entries.** AUTHOR., INSTALLATION., DATE-WRITTEN.,
  DATE-COMPILED., SECURITY., REMARKS.: the text to the next paragraph or
  division is a comment-entry, any characters. The tokenizer saw the
  apostrophes in GLACRPT's prose as an unterminated literal. A pass over the
  source lines before tokenizing (`strip_comment_entries`) keeps the header
  and blanks the rest. (cment)
- **#36 ALTER inside IF/ELSE.** The prescan gathered ALTER targets only at
  sentence start; GENSRT19's polyphase merge alters inside IF/ELSE. ALTER
  is reserved, so it is matched anywhere. GENSRT19 compiles now. (alter3)
- **#34 ASSIGN.** RM's device word before the name (RANDOM, PRINT, DISK,
  ...) is accepted and ignored when a name follows, refused when bare as
  before; a group item may name the file (it is alphanumeric by the
  standard's rules). (assign2)
- **#35 STOP RUN {identifier | RETURNING n}.** One numeric operand into the
  exit status; SJCLCODE ends every program with STOP RUN JCL-CODE and the
  menu scripts branch on it. (stoprc)

Gate: cobol suite 109/109 (oracle agrees on the four); NIST NC 95 compile,
4375/4384, 0 fail, unchanged. What the corpus hits next is the positioned
DISPLAY/ACCEPT (#32, #33): GLACRPT, GLPKACT, CRGENTBL, GENACT all stop there.

### 29. The Open Systems suite: RM/COBOL positioned DISPLAY/ACCEPT — RESOLVED 2026-09-05

GitHub #32 and #33, the wall the suite hit after the four small ones (item
28): 121 DISPLAYs and 86 ACCEPTs in GL alone carry LINE / POSITION / ERASE /
PROMPT / SIZE / HIGH / LOW / UPDATE / NO BEEP, and the 1983 utilities use
`AT rrcc`. Lowered to a one-statement screen on the Stage 58 runtime, as the
issue proposed; docs/screen.md has the shape. Two things the corpus taught on
the way: `ERASE SCREEN` (GLACGL) is the whole-screen form, and a plain
DISPLAY after positioned ones goes to the next line at column 1 (RM's rule,
and GLTRIAL's "ACCOUNT NUMBERS FROM" depends on it). The look-ahead that
decides a DISPLAY is positioned must stop at an enclosing statement's
phrases -- the suite's compute and lineseq tests caught `NOT ON SIZE ERROR`
and `NOT AT END` being read as SIZE and AT.

GL: 30 of 32 programs compile (17 with the clauses stripped by hand before,
0 as written); the two left, GLPKACT and GLPRTACT, compile and fail at link
on CALL "SH" and CALL "VAR:ID", the tag-file sort path, which is the
corpus's own next item. Gate: suite 110/110 (rmscreen pins the ANSI stream,
no oracle: screens need a tty); NIST NC unchanged.

### 30. The Open Systems suite, running: two runtime rules the paper needed (2026-09-06)

With every GL program compiling (item 29 and the tag-file rewrite in the
corpus), the first paper -- GLPRTCHR's chart of accounts -- needed two things
of the runtime that no test had asked for:

- **STOP RUN closes open files.** GLPRTCHR's tie-up closes the master and
  leaves PRINTER.TXT open; RM/COBOL and GnuCOBOL close it at STOP RUN, we
  dropped its buffered pages. libcob keeps a registry of every file OPENed
  and closes what is still open in `cob_stop_run` (and on the exit below).
- **End of input on a screen ACCEPT ends the run.** An RM program
  re-prompts on a bad answer; a scripted run whose keys run out therefore
  repainted its prompt forever (a 330MB stream before it was caught).
  `scr_key` at EOF closes the files, restores the terminal, says
  "end of input on ACCEPT" on stderr and exits 2.

The chart print now produces its dated heading page and detail lines from
a master built by GLACGL under a key script. The suite is unchanged (110).

### 31. The Open Systems suite: AP and IN (2026-09-06)

The two modules together, as the user asked (AP interacts heavily with IN).
Census as they stood: AP 12 of 38 compile, IN 3 of 17; three walls, none a
screen matter: a bare `END PROGRAM.` (26 programs), COMP-1 with a PICTURE
(RM's two-byte binary integer, a float elsewhere), and a doubled period
after a VALUE that RM's reader let through. With those, and `UNLOCK` as a
no-op (RM record locking, one user here), and the tag-file rewrite carried
over to their copybooks -- two sort copybooks per module, one on OSTAGS as in
GL and one on APTAGS/INTAGS -- plus OASIS's `PIN.COMMAND:S` (the task number,
a lock owner) made a constant in SAPTRCTL: AP 37 of 38, IN 16 of 17. The one
left in each is CPINVBIL, whose SEXINV copybook exists nowhere in the corpus,
and APENTER needs its subprogram APENTP compiled beside it. Test rmend (no
oracle: GnuCOBOL's COMP-1 is a float in every dialect it offers); UNLOCK in
assign2, where the oracle agrees. Suite 111/111.

### 32. An indexed file left open at STOP RUN lost its writes (2026-09-07)

Item 30's close-at-STOP-RUN registered files at the two sequential OPEN
sites; the indexed OPEN takes its own path and was never registered, so a
program that left an indexed file open at STOP RUN (or at the end-of-input
exit) lost every record and key it had written: the record cache and the
B+tree pages are written at close. GENACT, the suite's table editor, is the
first program to do it -- it confirmed the write, returned to its key
prompt, and the tables file stayed empty; six lines reproduce it. Every
file is now registered at `cob_open`'s entry, whatever its organisation.
Suite 111/111.


### 33. AR module census and cycle (~/open/ar, 2026-09-07)

51 programs, all compile after the same rewrite as gl/ap/in (tag-file shell
calls, the OASIS task number, SD SORT-TAGS in the ten sorting programs;
arenter builds with arentp+arenter2, arpymts with arpymts2).  `ar/s32/run.sh`
runs the whole daily cycle from empty files and pins 22 papers, including the
cash receipt applied to the invoice and the G/L journal with both postings.
No compiler or runtime change was needed.  Two things the screens taught,
recorded because every key script depends on them:

- SERRORS' `900-DISPLAY-MESS` ACCEPTs a key after every message, so an
  `<< INVALID ENTRY >>` eats the keystroke that follows it; a wrong key
  cascades.  The pressed key is then run through `900-CHECK-COMMAND`, which
  is how the operator leaves a forced field: a junk entry, then `M`, then `Y`.
- ARTRAN (relative) carries a per-task backout slot in its control record:
  the first line item write marks the transaction, the totals write clears
  it, and the next program that opens the file (SARTRCTL) deletes a marked
  transaction.  A key script one `P` short of the totals loses the invoice
  silently -- the journal pick then reports `**NO RECORDS PICKED**`.  It is
  the system working as designed, not a file-system fault (a six-line
  relative-file replay of the read/rewrite/write pattern is exact).

The date mask in all four harnesses now also takes `Z9/99/99` headings
(` 9/07/26`), which the AR reports print; gl/ap/in were re-pinned with it.

### 34. SO module: reconstituted from source alone (~/open/so, 2026-09-07)

19 programs, all compile after the module rewrite (SSOCNTRL's task number a
constant, TGSOSORT sorting SOTAGS in COBOL).  The corpus note said SO could
not run without its menu scripts and a sample order file; neither was
needed: the create program builds the order file, and the cycle order is
legible from the program names.  `so/s32/run.sh` pins 10 papers and closes
the suite's loop: the order posts into AR's open invoices, the inventory and
the G/L journal.  No compiler or runtime change.  Two things learned:

- The entry program's screen is chosen by SWITCHES 1 and 3, not by its own
  menu: switch 3 on is the verify pass that marks an order shipped.  Its
  "ORDER # TAKEN" message is reached by falling through 135-CHECK-ORDER-NO
  when the order's totals record exists -- it means "complete", and I
  chased it as a compiler bug (a replica of the loop is exact) before
  reading the fall-through.
- The SD SORT-TAGS record I add to sorting programs was X(19) everywhere;
  SOTAGS, APTAGS and INTAGS are 24 bytes, and a sort through the shorter
  record drops the entry number: "BAD READ" in every program that reads
  the records the sorted tags name.  Now sized from the tag file.  The AP
  and IN papers were unaffected (one record each) but the sources are fixed.

### 35. PA module: the runtime's PERFORM exit, a stale record copybook (~/open/pa, 2026-09-07)

49 programs compile (SPACHKR.COB is a copybook; PA941/PA9412 exist only as
RM object files, no source).  Two compiler gaps closed on the way: OTHER as a
declared data name, END PROGRAM as the last line without a period.
`pa/s32/run.sh` pins 17 papers: the time ticket through the posted check,
the registers, the withholding reports, the W-2 and a balanced G/L journal.
Three findings, one of them a runtime rule:

- `cob_perform_exit` checked only the top frame of the perform stack.
  PAPOST's 745-READ-TABLE does `INVALID KEY GO TO 750-GET-TABLE-EXIT` from
  inside `PERFORM 745` while `PERFORM 705 THRU 750` is active: the inner
  frame is abandoned, 750's own return was refused, and control fell through
  760..800 into the error copybook ("<< INVALID ENTRY >>" after POSTING).
  The exit now searches down the stack for its range and drops the frames
  above it -- the per-paragraph return slots of the classic runtimes, which
  this code was written against.  Test `perfexit`.  It never showed in
  production because the path only opens when a company's FICA exclusion
  table is missing.
- `SPACHK` (the FD copy of the check record) names two year-to-date cells
  where `SPACHKR` (the LINKAGE copy) still had one FILLER; every routine
  taking the record by LINKAGE saw the withholding group four bytes early,
  so PAAUX zeroed the wrong cell and the auxiliary withholding kept its
  space-fill, printed as 20202.02 (three spaces and a comma, read as packed
  decimal).  SPACHKR aligned in the corpus.
- Open, not fixed: a positioned DISPLAY of a numeric item (COMP-3 here)
  shows its raw bytes; RM converts it.  Seen only through my own debug
  displays, no corpus statement found relying on it yet.

### 36. JO module, and the suite complete (~/open/jo, 2026-09-07)

21 programs, all compile after the module rewrite (its tag file is named
TAGS, 24 bytes).  `jo/s32/run.sh` pins 8 papers: the job and phase, the
adjustment log with the overhead it accrues, the four reports, and the
accrued overhead posted into the G/L journal.  No compiler or runtime change.
With it the Open Systems suite is complete on SLOW-32: gl 4, in 7, ap 12,
ar 22, so 10, pa 17, jo 8 -- 80 papers from seven harnesses, every one
produced from empty files by the programs themselves.  What the suite cost
the platform, in order: two runtime rules (ISSUES-30), an indexed-file
registration (32), the tag SD sizing (34), the PERFORM exit rule and two
parser gaps (35).  What it cannot do: PA941 (object only), CPINVBIL (a
copybook the corpus never had).

### 37. Abandoned PERFORM frames accumulated (GLENTER's S command, 2026-09-07)

A GO TO out of a performed range to somewhere outside every range -- the S
command in GLENTER's 600-GET-ENTRY, and 900-BRANCH-S/X in the errors
copybook, a routine idiom across the suite -- left its frame on the perform
stack for good; 300 of them and the runtime stopped with "PERFORM nesting
too deep".  The push now replaces an existing frame for the same range and
drops what sat above it (an active range cannot be performed again, so those
frames are abandoned).  The stack is bounded by the number of distinct
ranges, as the classic per-paragraph return slots are.  Test `perfgoto`.
Raised by an outside review of GLENTER.COB that read the idiom off the page.

### 38. ~~Report Writer pages are taller than majesty's C++ generator~~ — GitHub issue 76, RESOLVED 2026-09-10

Comparing `reports_cpp/` to `reports_cobol/` (same data) and both through
virtual1403 (green bar, 132 columns): C++ and MVS 3.8j look right; COBOL 85
has more lines, more pages, and headings that sit lower after the first page.
The five printers (C++, two dBase, MVS 74, COBOL 85) used to agree on page
*shape* — not byte-identical, but the same 61-line numbered band. That
agreement broke. It did not break because someone bumped PAGE LIMIT.

The 85 RDs still copy the 74 numbers: PAGE LIMIT 61 on chart/journal/activity
and **66** on the balance sheet and profit-and-loss. 66 is the 1403 form
(11" at 6 LPI). 61 is the printable band after JES2's 5-line top skip —
virtual1403 `default-green`, C++ `lineCount >= 60` then pad to 61, dBase
pad-to-60. MVS keeps PAGE LIMIT 66 because ASA skip-to-channel-1 *is* that
skip: 66 names the form, not 66 newline records.

**Where the deviation came from.** The 74 print file is `ASSIGN TO UT-S-PROUT`
(printer, ASA). The 85 print file became `ORGANIZATION IS LINE SEQUENTIAL`
the day the reports first produced populated output (majesty, 2025-05-20:
gl022 `03a5b1d`, gl042 `0dad6e8`; gl043 born that way 2025-12-04). GnuCOBOL's
RW on a line-sequential disk file materializes PAGE LIMIT as physical
records, no form feed — measured on majesty's `.prn` and written into
`docs/report-writer.md` at Stage 7 (`8def6bd5`). s32-cobc then matched that
geometry byte-for-byte (Stage 7 on the 61-line reports, Stage 15 `68e23237`
on the 66-line ones). So the compiler did not drift from GnuCOBOL; it
**locked in** a GnuCOBOL-on-LINE-SEQUENTIAL reading of 66 that MVS-on-ASA
never had.

On virtual1403 `default-green` that is fatal: skip 5, then 61 numbered lines.
A 61-record page (C++, 85 chart/journal/activity) fills the band and the
next heading is the first record of the next form. A 66-record page walks
five pad lines onto the next form, so page 2's heading starts five lines
lower, page 3 lower still. Occupancy: the two 66-line reports are +5 per
page vs C++ (66 vs 61; two-page files 132 vs 122). The 61-line RDs still
match C++ in *count*.

Grounded in the Drive PDFs (2026-09-10, `default-green`, report-text Y
ignoring the form's line numbers): MVS `2026-08-J663-BATCH.pdf` — 87 of 88
pages start at 58pt (~line 6). C++ every page of every report at 58pt.
COBOL 85 PAGE LIMIT 61 reports stay at 58pt. The 66-line ones: page 1 at
58pt, page 2 at 118pt (exactly +5 lines at 6 LPI), then an empty form.
dBase is not part of this comparison.

`tests/compare_reports.sh` strips headers, so a green data compare never
saw the shape break. Stage 32 (`9accf951`) the next day changed when
TERMINATE pads (`page_started` vs `page_counter`); that is a possible
last-page tweak, not the 66-vs-61.

Adopted: change the 85 RDs, not the page engine. Majesty `gl042` /
`gl043` are now `PAGE LIMIT 61` with the page heading packed to the C++
line slots (`FIRST DETAIL 5` / `6`). GnuCOBOL 4.0 and s32-cobc were
already byte-identical on the old 66-line RD (LINE SEQUENTIAL, no
ORGANIZATION, and `ASSIGN TO PRINTER` alike); MVS keeps 66 because ASA
still means form size. Remaining occupancy diffs on the balance sheet
are body blanks (C++ extra `OutputLine` after a class), not page length.
Details on GitHub issue 76.

### 39. COPY of an uppercase copybook failed on a case-sensitive filesystem (~/open on Linux, 2026-09-15)

The tokenizer lowercases every word, so `COPY SCONFIG.` looked for
`sconfig`.  On the MacBook's case-insensitive filesystem that found the
Open Systems copybook `SCONFIG`; on Linux every program in ~/open was
refused with "COPY: cannot find 'sconfig'".  `copy_open` now tries the
text-name upper-cased as well, in each directory and with each extension
(a literal text-name still arrives as written).  Test `copyupper`, on
`tests/copy/SUPPER`.  The seven ~/open harnesses run clean on Linux with
this, every pinned report byte-identical.

### 40. A clear inside a screen update never reached the terminal (2026-09-15)

Entering a program from the Software Fitness Program's shell menus left the
menu on the screen under the program's fields.  The term service paints a
positioned DISPLAY inside begin_update/end_update and emits, at the end, the
difference between its shadow and the snapshot it took at the start; a
`TERM_CLEAR` inside the bracket only blanked the shadow, and the diff of blank
against blank is nothing.  The shadow cannot know what another process left
on the physical screen -- the shell's menu -- so a clear is not a diff.  It is
now recorded during the update and replayed on the terminal at end_update,
ahead of the repaint (`ESC[2J ESC[H`, or the cursor position and `ESC[J` /
`ESC[K` for EOS / EOL), and the snapshot is blanked over its range so the
repaint covers it.  The six screen tests' expected files changed by exactly
the inserted clears and the repaints after them; rendered to a screen image
they are identical to before, except `screen3`, whose old image kept a stray
`]` past a BLANK SCREEN -- the bug, pinned.  The same change is in the qemu
backend's copy of the service (not built here).  Regression and differential
suites agree across the interpreter, fast and DBT.

Separately, an RM program that never clears (GLENTER opens with an ERASE EOS
from line 16 and paints over lines 1-13) relied on runcobol clearing the
screen at start; the deployment's runcobol shim does that, not the runtime.

### 41. The compiler reported the first error and stopped (2026-09-26, fixed 2026-09-28)

Every diagnostic in `s32-cobc` goes through `die_at()`,
which prints one message and calls `exit(1)`: **582 call sites in 9,508
lines**. A program with three mistakes yields one message, the author
fixes it, compiles again, and finds the second. Nothing is wrong with
any individual message -- the refusals are precise, and gate 3 of the
harness checks thirteen of them -- but the compile is one error long.

Found by comparing against cobc370, which solved it as its own #41 on
2026-09-26, and whose recipe transfers as a design even though no line
of its code can:

- an error inside a **sentence** or inside a **data entry** is reported
  and parsing resumes at the next period (or where it stands, when the
  error came after the period, as an entry's checks do);
- the failed sentence or entry is dropped;
- **nothing is generated once anything has failed**, so a partial
  object never escapes;
- a cap (thirty there) stops a cascade from filling the listing;
- everywhere else an error is still the end: `setjmp` around the two
  loops, `longjmp` in the fatal path. The recovery is deliberately
  narrow, at the two places the language gives an unambiguous
  resynchronisation point.

Their fixture is `bad-multi`, three mistakes expecting three messages.
The same shape would work here, alongside the existing `tests/bad/*`.

Why it is not scheduled: no program has asked. The corpus compiles 56
of 56, CCVS-85 compiles 303 of 303, and the Open Systems suite is in;
a one-error compile costs an author iterations, not correctness. It
belongs on the list because 582 fatal sites is the kind of thing that
only gets more expensive, and because the two trees now disagree on a
point of craft where the other one is right.

**Fixed**, on cobc370's recipe. `die_at` counts the error and, inside
a sentence or a data entry, jumps back to the loop that set
`g_recover`; the sentence or entry is dropped and the parse resumes
after its period. Resynchronisation also stops at the next paragraph
or section header, or at a level number opening a line, so a missing
period costs one message, not the next sentence's too. VALUE clauses,
checked a record at a time when the data division is finished, recover
per record. Everywhere else an error is still the end, and a cap of
thirty stops a cascade. Once anything has failed nothing is generated:
the partial `.s` is removed (before, a refused compile left one behind).

Two measures keep the messages to one per mistake. A dropped data
entry leaves a `FILLER PIC X` at its level, so a group whose only item
failed stays a group; and its name, with every name the resync skipped,
is poisoned -- a later use drops its sentence without a message. The
`USE FOR DEBUGGING` refusal poisons the Debug module's registers the
same way. PERFORM and GO TO naming a paragraph that does not exist now
say so, rather than "is not a COBOL verb" and "GO TO without a
procedure-name".

Gate 3 now holds one line of `.expected` per error, requires exactly
that many, and fails a refused compile that leaves output behind. That
strictness found three cascades in the existing fixtures (rw-data,
rw-index, use-debugging), each fixed. New fixtures: bad/multi-error
(six mistakes, six messages), bad/missing-period, bad/no-paragraph.

Fuzzed against the Open Systems suite: 1,500 copies with one to six
words deleted, replaced or unpunctuated, compiled under AddressSanitizer
and UBSan -- no crash, hang, sanitizer report or leftover output. The
first round found a tokenizer bug older than this work: a period glued
to a number and a word (`.00-EXIT`, a mangled paragraph header) pushed
empty tokens until memory ran out. It is now the period error it
always should have been. Valid programs are unaffected: all 227
compilable Open Systems programs produce byte-identical assembly.

**Lesson.** The strict gate paid for itself on its first run: the
old one matched any line of `.expected`, so the three cascades it
found would have passed. And the fuzz found a real bug in the first
600 cases, in code nobody had touched -- recovery is what lets damaged
input reach the corners.

### 42. SORT/MERGE with more than one GIVING file wrote only the first (2026-09-05, found and fixed 2026-09-27)
The budgeted external sort (`xsort.h`, 2d76bc1c, 2026-09-05) made the
sorted stream something `xs_next` reads once -- runs merge off disk --
but `cob_sort_giving` still wrote each GIVING file by draining the
stream. The first file got every record and the rest got none. The
standard gives every record to every GIVING file; the in-memory code
before it had rewound per file, so the change was invisible until a
program named two.

CCVS-85 ST147A, a MERGE with three GIVING files, caught it: 12 of 26
with 14 failing, "PREMATURE EOF FOUND" from MRG-TEST-011 on. The suite
went from 8049 of 8160 with none failing to 8035 with 14. Nothing else
saw it: the harness had no two-GIVING program, the corpus has none,
and CCVS-85 was run by hand, so it sat for 22 days. Localized by
running ST147A under all three engines (identical, so not the engine),
then `git bisect` over `cobol/` with the current tools linked in.

Fix: GIVING opens and registers each file; `cob_sort_end`, which the
compiler emits right after the last GIVING, drains the stream once
and writes each record to every registered file, then closes them.
One pass, where the old code made one per file. `tests/free/sortgive2`
spills a SORT into two GIVING files and MERGEs those into two more; it
prints what GnuCOBOL prints, and on the unfixed runtime its second and
fourth files are empty.

**The lesson is the gate.** CCVS-85 runs in thirteen seconds. It is
now gate 5 of `tests/run-tests.sh`: the totals line must equal
`tests/ccvs-baseline.txt` exactly, a better total included, so a
change in either direction is recorded on purpose. No NIST tree is
reported as NOT RUN, never passed over.

### 45. `COB_CURRENT_DATE`: a fixed clock for reproducible paper (2026-09-27)
The Open Systems AR cash-flow paper drifted with the calendar
(ISSUES-44): an invoice takes the run date and ages against fixed dates
in the key script, so the same 60.00 lands in a different column
depending on the day the suite runs. Preservation needs the same paper
every day, and GnuCOBOL already names the answer.

`COB_CURRENT_DATE=YYYY/MM/DD` or `YYYY/MM/DD hh:mm:ss` now fixes the
clock that ACCEPT FROM DATE, DAY, TIME, DAY-OF-WEEK and FUNCTION
CURRENT-DATE read, through one function (`cob_clock`) where there were
two copies of the clock read. Every date field was measured against
GnuCOBOL 4.0 on four values -- an ordinary Monday, a leap day that is
a Thursday, a Sunday at a year boundary, the date-only form -- and
agrees. Where GnuCOBOL lets the real clock through, this one stays
fixed (section C). A malformed value is fatal with the value quoted:
quietly running on the real date would defeat the reason to set it.

`tests/free/fixclock` fixes 2024/02/29 23:59:59 through a `.env` and
prints what GnuCOBOL prints; without the variable it reads today and
fails. The harness now keeps a `.env` line whole (a value may hold a
space) and gives it to the oracle as well, which is what lets a fixed-
clock test have an oracle at all.

In `~/open` every module's `run.sh` sets
`COB_CURRENT_DATE=2026/09/07`, the day all seven modules' paper was
pinned, overridable from outside. **All 80 papers match** (AP 12, AR
22, GL 4, IN 7, JO 8, PA 17, SO 10), the cash-flow report included.

Correction: ISSUES-44 and its commit (a8ef2f12) report the Open
Systems suite as "100 of 101 papers"; there are 80, and the right
figure that day was 79 of 80. The count was not taken from the
harness output, and it should have been.

### 46. A print file is a line printer; the 91 inspection tests read (2026-09-27)
**The inspection survey.** CCVS-85's 91 visual-inspection tests are in
eight programs. Each was run here and under GnuCOBOL and the printed
output compared:

| programs | tests | result |
|---|---|---|
| SQ201M SQ208M SQ209M SQ210M | 24 | byte-identical to GnuCOBOL |
| SQ101M | 57 | all 64 of its layout claims hold (GnuCOBOL: 60) |
| SQ207M | 8 | all 12 of its layout claims hold (GnuCOBOL: 8) |
| SM106A | 1 | identical but for GnuCOBOL's leading blank line |
| NC114M | 1 | the thing to inspect is a compiler listing; none is produced |

SQ101M and SQ207M say where each of their lines must land ("THIS LINE
SHOULD BE 1 LINES BELOW AND 8 LINES ABOVE THE BRACKETING WRT-TEST
LINES", "SHOULD APPEAR AT THE TOP OF A NEW PAGE", "ONLY FIVE OF THE
LETTERS A AND B SHOULD BE JUMBLED"). `tests/sq101m-layout.py` renders a
print file as a printer would -- newline, form feed, carriage return
with overlay -- and checks each claim; gate 5 runs it on SQ101M.

**What it found.** The runtime wrote each record's newline with the
record, which is right only while every WRITE is AFTER. A printer's
cursor sits on the line last printed: AFTER n moves it and prints, BEFORE
n prints where it is and then moves it. So BEFORE after AFTER landed a
line too low, AFTER after BEFORE a line too high, and ADVANCING 0 could
never overprint, the newline being on disk already. SQ101M held 38 of
its 64 claims. Also found: a count taken from an item holding zero was
encoded as -1, which is PAGE (SQ101M's LONG-ZERO: three extra form
feeds); and the zero-advance flag was never reset at OPEN nor its line
ended at CLOSE.

**Fixed.** The runtime keeps the printer's cursor in the cob_file word
that held the flag (`pr_state`, no layout change): a record's newline
goes out when the cursor next moves, and printing on a line that
already has ink writes a carriage return first -- the user's choice,
over GnuCOBOL's appending, because a printer, a terminal or col(1) lays
the records over each other as the text means. The compiler marks a
BEFORE phrase (before = -3) so BEFORE 1 is no longer AFTER 1 to the
runtime, and maps a zero count from an item to zero lines. Streams of
AFTER writes come out byte for byte as before.

**Preserved programs.** Five Open Systems programs use ADVANCING 0 --
ARINVCS, ARSTMTS and SOINVCS for a forms-alignment line, APPRTCKS for
the check form, SOPSLIP for the slip header; their paper is unchanged
(the harness answers the alignment prompt yes the first time, so
nothing is overprinted). One paper moved, PA's W-2 forms, and the move
is the fix: the form is 22 lines (W2-LINE-1..22) and PAW2 prints its
alignment line BEFORE ADVANCING 21, so the first real form belongs one
form length below it. The new paper puts the first control number 66
lines below the alignment's, three forms exactly; the pin had it at
65. Re-pinned in `~/open`. 79 of 80 papers unchanged; majesty passes.

**Read against the text (2026-09-27, FIPS PUB 21-2; docs/oracles.md).**
WRITE, Sequential I-O general rule 15, page VII-54: "If the ADVANCING
phrase is not used, automatic advancing will be provided by the
implementor to act as if the user had specified AFTER ADVANCING 1
LINE"; (15)c, a zero count performs no repositioning; (15)e and f, BEFORE
presents the line and then advances, AFTER advances and then presents
it. That is the model above, word for word. So GnuCOBOL's WRITE with
no ADVANCING, which does not advance at all, departs from the text as
well as from SQ101M tests 19 and 20; with the page reference that is
now a clean report for upstream, not yet filed.

Where a plain print file's first line falls is not in the text. The
nearest rule is LINAGE's (general rule 9, VII-28): LINAGE-COUNTER is
the line the device is positioned on, and OPEN OUTPUT sets it to one.
Both compilers keep that counter and still write a LINAGE file's first
AFTER 1 record on the file's first line, with no blank line above it;
how a file represents the page above the first record is left to the
implementor ((15)h, physical pages). This runtime's plain print files
start the way both compilers' LINAGE files do. GnuCOBOL's plain print
files alone start a line lower, unlike its own LINAGE files. Ruled:
kept.

### 47. Obsolete 1985 elements the registry missed; MOVE ALL "digits" computed the wrong value (2026-09-28, fixed the same day)
Found checking docs/behavior-points.md against the texts (the 1985
Obsolete Language Element List, FIPS PUB 21-2 XVII-81 ff, and
ISO/IEC 1989:2023). The registry's own claims held or were corrected
there; these are what it did not cover. No preserved program uses any
of them.

- **`MOVE ALL "digits"` to a numeric item computes the wrong value.**
  Obsolete element 2, which a conforming implementation must still
  support. The 1985 list gives X3J4 interpretation B-23's results:
  `MOVE ALL "99"` and `MOVE ALL "123"` to `PIC 99V99` give 99.00 and
  31.00 -- the literal repeated to the receiver's size in characters
  ("1231") and moved as an integer, truncating on the high-order side.
  Here they give 99.99 and 11.11; GnuCOBOL gives 99.00 and 12.00, so
  it agrees with the text on the first and not the second. No program
  in majesty or the Open Systems suite uses the form.
- **Accepted silently, not yet behavior points**: that `MOVE ALL`
  (item 2), `RERUN` (item 5), `MULTIPLE FILE TAPE` (item 6), and
  debugging lines with `D` in column 7 (item 18). `-warn-74` should
  name each, as it does ALTER and the rest of class O.
- **`USE FOR DEBUGGING` is refused with a parse error**, "expected
  'after', found 'for'", where the rule is a message naming what is not
  implemented: the Debug module, obsolete element 18.

**Fixed.** `MOVE ALL` to a numeric or numeric-edited item now repeats
the literal to the item's character positions and moves the result as
the alphanumeric literal it is (IV-11): 99.00 and 31.00, the text's
values; free/moveall, with GnuCOBOL's 12.00 kept as a documented
divergence (section C). A one-character literal changes too: `ALL "1"`
to `99V99` is 11.00 by the same rule, where the fill gave 11.11.
`-warn-74` names the four as BP-O9 to BP-O12. Debugging lines are
compiled under `WITH DEBUGGING MODE` and are comments without it (VI-10,
SOURCE-COMPUTER rules 4-5; they were always dropped before):
fixed/dbgmode and fixed/dbgoff. `USE FOR DEBUGGING` is refused naming
the Debug module (bad/use-debugging). With these, every item of the
1985 obsolete list is a point, refused with a message, or (item 1) moot
on ASCII. The Open Systems suite re-audited: none of the four occurs.

**Lesson.** The registry was built from the constructs the corpora
carried, and a list assembled that way is exactly as complete as the
corpora. Checking it against the text's own list found four more in an
afternoon, and a wrong value under one of them.

Also recorded from the same check: 2023 removed `CLOSE ... WITH LOCK`
and file status 38 (Annex E), both implemented here, and marks the
fixed-form continuation indicator obsolete (Annex F) -- matters only to
a future 2023 switch, and the preserved corpora all rely on it.

### 48. A CALL could break the caller's PERFORM; USING arguments lost to the prologue (found and fixed 2026-09-28)

Found reading the call path before Stage B's `RECURSIVE`, which needs
each activation to own its state. Two defects in plain COBOL 85, both
in every build until now, neither hit by majesty, CCVS-85 or the Open
Systems suite.

- **A CALL inside a performed paragraph could break its PERFORM.**
  The PERFORM stack is one runtime stack keyed by paragraph id, and ids
  are numbered from 1 in every program. When the called program
  performed a paragraph with the same id as the caller's active one,
  `cob_perform_push` took the caller's frame for an abandoned range and
  dropped it; back in the caller, the paragraph's end found no frame
  and fell through into the next paragraph. Now each activation calls
  `cob_perform_enter` after its prologue, keeps its frames above that
  base, searches no lower, and drops them all at `cob_perform_leave` on
  return. free/performcall.
- **A USING program lost its arguments** when its prologue made a call
  first -- `IS INITIAL` (the CANCEL routine), `DECIMAL-POINT IS COMMA`,
  `CURRENCY SIGN`, or a `PROGRAM COLLATING SEQUENCE`. The addresses in
  r3 onward were stored into the LINKAGE cells after those calls had
  clobbered them, and the program wrote to address 1. They are stored
  first now. free/usingfirst.

GnuCOBOL agrees with both tests' output. Neither construct is rare --
a performed paragraph that CALLs is ordinary COBOL -- and the first
needs only a same-numbered paragraph on the other side, so the gap in
the corpora is luck, not rarity.

**Lesson.** Reading a mechanism for what the next feature needs is an
audit of what it already does. Both bugs were on the call path; the
corpora passed the first because of which paragraph numbers happened
to line up.

### 49. Stage B, first module: `RECURSIVE` and `LOCAL-STORAGE` (2026-09-28)

The first COBOL 2002 module, and the first use of `-std=2002`.
docs/standards.md has the summary; this is the record.

**What an activation owns** was found by an audit of every piece of
static state the generated code and libcob keep. Three would be
clobbered by a second activation of the same program: the LINKAGE
address cells, a FILE STATUS pointer stored into the file block when
the status item is in LINKAGE, and the `PERFORM ... TIMES` counters
(one static word per statement). The PERFORM return stack was the
fourth, fixed first as ISSUES-48. Everything else is statement-scoped
(no CALL can happen inside it), constant, or program state the
standard makes static: file connectors, sort files, reports,
index-names (and ALTER state, where that 85 element is still accepted).

**How.** Under `-std=2002` every program gets an activation descriptor:
its active count, whether it is RECURSIVE, its name, the words an
activation owns, and each LOCAL-STORAGE record's cell, initial image
and size. `cob_act_enter` checks for re-entry, saves the words into a
malloc'd block, copies each LOCAL-STORAGE image into the block and
points the record's cell at it; `cob_act_leave` restores the words and
frees the block. LOCAL-STORAGE is reached through its cell the way
LINKAGE is (one load), so a local item's address can be passed BY
REFERENCE and stays that activation's -- copying locals out and back
instead would have broken exactly that, which recshape tests. The
argument registers wait in frame slots while `cob_act_enter` runs.
Only a RECURSIVE program's words are saved, since only it can be
re-entered.

Tests, all agreeing with GnuCOBOL `-std=cobol2002`: 2002/recfact
(factorial; fresh LOCAL-STORAGE per activation, LINKAGE intact after
the inner call), 2002/recshape (a TIMES loop around a recursive CALL,
7 activations; a local passed BY REFERENCE to a program that re-enters
the caller), 2002/localfresh (LOCAL-STORAGE fresh on every CALL of a
program that is not recursive, WORKING-STORAGE kept), 2002/recnot
(EC-PROGRAM-RECURSIVE-CALL). Refusals: bad/recursive-85,
bad/local-storage-85, bad/std2002-initial-recursive (2023 11.10.3
rule 5). The harness runs tests/2002 under `-std=2002`; a bad fixture
named `std2002-*` compiles under it too.

`-std=85` output is byte-identical to before on all 227 Open Systems
programs and the majesty programs, with one intended exception found on
the way: ASSIGN, file DEPENDING ON, RELATIVE KEY, LINAGE and CRT STATUS
items were checked against LINKAGE only, and an EXTERNAL item there
compiled to the address of its cell instead of its data. They are now
refused for LINKAGE, LOCAL-STORAGE and EXTERNAL alike; no program in
any corpus used one.

**Lesson.** The audit came before the design, and the design came out
smaller for it: a descriptor per program and one block per activation,
rather than frame-relative addressing threaded through every
statement.

### 50. Stage B, second module: user-defined functions (2026-09-28)

`FUNCTION-ID`, `REPOSITORY` and function invocation under `-std=2002`.
docs/functions.md has the design; this is the record.

**Driven by real code.** majesty's date family was written with COBOL
2002 functions and rewritten to 85 subprograms in its e69e98b. The
originals, taken from its history unchanged -- twelve functions in
seven files, invoked bare inside ADD, SUBTRACT ... GIVING, IF and MOVE,
with literal and expression arguments -- now build with `-std=2002`,
and `tests/majesty-functions.sh` requires them to match GnuCOBOL byte
for byte: jerm's 400,001 lines, and the gltrans trio (crgltrans and
ldgltrans as majesty has them, the original exgltrans) over 3,000
synthetic transactions. Each build keeps its own indexed format, so the
three run as a set.

**Decisions.**
- The external repository (the user's choice of three): a function's
  compile writes `name.s32fn`; a caller reads it; `-fnsig` writes only
  signatures, and `compile.sh -std=2002` runs it over every input first.
  Call sites stay compile-time specialized. The rejected alternatives
  were GnuCOBOL's runtime descriptors (the generic path standards.md
  weighs against) and prototypes only (majesty's originals would need
  edits).
- A function call is made where it is evaluated: at parse time outside
  conditions, recorded on the condition and made at each evaluation
  inside one. VARYING's BY and an AFTER's FROM are refused for now.
- Arguments follow 8.4.3.2.4 rule 5 and 14.8.2.3: BY REFERENCE for an
  identifier, which must conform; BY CONTENT for the rest, converted
  into a copy described like the parameter. One extension, registered
  in class E: same-size two's-complement binary integers conform
  (majesty's holidays passes a SIGNED-INT to a COMP-5 parameter).
- The RETURNING item must be in LINKAGE (14.2.2 rule 5). The first cut
  allowed WORKING-STORAGE with a copy-out; GnuCOBOL refused the test,
  the text agreed with it, and the copy-out went.

**Found on the way.**
- ADD, SUBTRACT, MULTIPLY and DIVIDE with GIVING scanned their operands
  with no code and kept the scan's operands, so a function there was
  never called -- jerm's first run computed today as -584389, the bare
  epoch offset. They re-parse
  now, and a guard makes any scanned result that reaches code an
  internal error rather than a silent wrong value.
- libcob's evaluation stack stopped at 32 entries and the PERFORM stack
  at 256 frames: fixed limits that recursion turns into depth limits.
  Both grow now (2002/userfndeep recurses 1000 deep through COMPUTE).
- GnuCOBOL 4 passes a function's BY CONTENT numeric arguments wrongly:
  `twice(a + 4)` gives 0, `twice(-7)` 1400, `clamp(42, 0, 100)` 100
  (docs/oracles.md). A candidate for a clean upstream report.

Tests: 2002/userfn (in-source functions: bare, FUNCTION, conditions,
PERFORM UNTIL, EVALUATE WHEN, GIVING, recursion, an alphanumeric
result, a function calling a function), 2002/userfnx (a separately
compiled library through the repository files), 2002/userfndeep.
Refusals: function-id-85, and std2002-fn-argcount, -byref,
-returning-ws, -nosig, -varying-by. `-std=85` output is byte-identical
on all 227 Open Systems programs; majesty PASS; Open Systems paper
unchanged.

### 51. Free-form reference format checked against the text (2026-09-28)

Stage B's free-form row: free format was implemented long ago as a
GnuCOBOL-style extension, and had never been read against COBOL 2002's
own rules (2023 6.2-6.5, the same text). Probing each rule found two
gaps and one diagnostics bug:

- **Floating literal continuation** (`"-` / `'-` ending an unterminated
  literal, the next line resuming after a matching quote; 6.2.3, 6.4.2)
  was not recognised. Implemented in the line reader for both formats,
  comment and blank lines skipped between the parts, a missing quote on
  the continuation line refused (6.2.3.2 rule 6). Under `-std=85` it is
  refused by name.
- **`>>SOURCE FORMAT IS FIXED | FREE`** (7.3) was not recognised; it now
  switches the format for the rest of the text. Other directives are
  refused by name (conditional compilation is its own module), and all
  of them under `-std=85`. No corpus uses one.
- An error raised while lines were being read named the file `?` (the
  tokenizer's file was not set yet); the reader names its own file now.

Floating comments (`*>`), already accepted in both formats, conform.

Tests: 2002/freeform (both quote forms, a comment line inside a
continued literal, a three-line literal with a doubled quote, a switch
to fixed form and back), agreeing with GnuCOBOL -std=cobol2002.
Refusals: directive-85, floating-continuation-85,
std2002-directive-unknown, std2002-continuation-quote. `-std=85` output
byte-identical on all 227 Open Systems programs; CCVS-85 unchanged;
majesty PASS; tests/majesty-functions.sh PASS.

### 52. The COBOL 2002 intrinsic functions (2026-09-28)

A survey of the three texts: the 1989 set's 42 functions were all here;
2002 adds 33, 2014 eleven more, 2023 seven more, and no edition removes
any. Of 2002's 33, eighteen need no other module and are implemented under
`-std=2002`: ABS, EXP, EXP10, PI, SIGN, FRACTION-PART, HIGHEST- and
LOWEST-ALGEBRAIC, BYTE-LENGTH, YEAR-TO-YYYY, DATE-TO-YYYYMMDD,
DAY-TO-YYYYDDD, TEST-DATE-YYYYMMDD, TEST-DAY-YYYYDDD, NUMVAL-F,
TEST-NUMVAL, TEST-NUMVAL-C, TEST-NUMVAL-F. The other fifteen need the
NATIONAL or BOOLEAN module, exception handling, locales or the ISO/IEC
14651 ordering, and are refused naming it; 2014's and 2023's are refused
naming their edition; an unknown name is "not an intrinsic function".
Under `-std=85` every 2002 name is refused as 2002.

Each function follows its clause's returned-values rule, checked against
the text's own examples (YEAR-TO-YYYY (4, 23) in 1995 is 2004; (98, -15)
in 2008 is 1898; DATE-TO-YYYYMMDD (851003, 10) in 2002 is 19851003).
NUMVAL, NUMVAL-C and NUMVAL-F's argument formats became one scanner in
libcob that the TEST- functions report from, so a string the test calls
valid is always one the conversion reads.

2002/intr2002 exercises all eighteen, ten strings through TEST-NUMVAL
among them, and agrees with GnuCOBOL -std=cobol2002 except for
TEST-NUMVAL-C with a currency string, where GnuCOBOL reports an error
its own NUMVAL-C does not see (docs/oracles.md). Found on the way:
NUMVAL-C's argument-2 was parsed and ignored since the 1989 module
landed; it is honoured now. Refusals: intrinsic-2002-85,
std2002-intrinsic-2014, std2002-intrinsic-module. Harness 162/162;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 53. Exception handling, part one: TURN, RAISE, exception declaratives (2026-09-28)

Stage B's exception module is too large for one row; this is its
foundation. Table 13 (2023, 158 exception-names: EC-ALL, 24 groups, 133
conditions -- 73 fatal, 38 nonfatal, 22 implementor-defined, taken here
as nonfatal) is in the compiler, transcribed from the text and checked
against its own counts; EC-USER-suffix names are the user's.

- **`>>TURN name ... CHECKING ON [WITH LOCATION] | OFF`** (7.3.25). The
  reader keeps the directive as a token; the parser applies it on
  reaching the next statement, so it holds from that point in the
  source. EC-ALL and a group name expand to their conditions,
  EC-I-O-WARNING excepted (rule 4); EC-ALL and EC-USER also turn on user
  names first met later. Default: all off (rule 1). TURN for one file
  is refused for now.
- **`RAISE EXCEPTION name`** (14.9.29), a level-3 name. Checking off at
  that statement: nothing is compiled. On: the last exception status is
  set, the declarative that applies is performed -- the program's USE
  for the name, else for its group, else for EC-ALL -- and a fatal
  condition then ends the run (14.6.13.1.3 rule 5; libcob reports it and
  exits 3). All of this is decided at compile time.
- **`USE AFTER EXCEPTION CONDITION name ...`** (also `EC`; 14.9.49 format
  3). FILE, and EXCEPTION OBJECT, are refused.
- **EXCEPTION-STATUS** and **EXCEPTION-STATEMENT** (15.33, 15.32), the
  statement's name only when WITH LOCATION turned checking on;
  **`SET LAST EXCEPTION TO OFF`**. RAISE and RESUME are verbs only under
  `-std=2002` (RESUME, optional since 2014, is refused).

Open, for the next parts: the statements that raise conditions
themselves (EC-SIZE where no ON SIZE ERROR is written, EC-BOUND-SUBSCRIPT
and -REF-MOD, EC-I-O from the I-O status, EC-PROGRAM-NOT-FOUND ...),
each compiled in only where checking is on; TURN for one file;
EXCEPTION-LOCATION and EXCEPTION-FILE, whose results are as long as
their contents (the function machinery has fixed widths); and 2023's
exception-checking PERFORM (E.2 item 19).

No oracle: GnuCOBOL 4 warns that USE AFTER EXCEPTION CONDITION is not
implemented and runs past a fatal RAISE. 2002/ecraise and 2002/ecturn
are reviewed against the text. Refusals: raise-85, use-ec-85,
std2002-raise-level2, std2002-ec-unknown, std2002-turn-file.
std2002-intrinsic-module now uses CHAR-NATIONAL, since EXCEPTION-STATUS
exists. -std=85 byte-identical on all 227 Open Systems programs;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 54. Reference modification of a function result was refused (2026-09-28, fixed the same day)

`FUNCTION CURRENT-DATE(1:8)` fails with "expected a statement, found
'('". X3.23a-1989 gives reference modification of a function-identifier
in its own format (FIPS PUB 21-3, the reference-modifier format:
FUNCTION function-name-1 [(argument-1 ...)] (leftmost:[length])), so
this is a COBOL 85 conformance gap, and a common idiom in real code.
Found writing 2002/ecturn. No corpus program uses it (checked: majesty,
Open Systems); CCVS-85 does not exercise it.

**Fixed.** After an alphanumeric function, a `(leftmost:[length])` with
literal positions is a reference modification: the function is
evaluated at its full width and the operand becomes the part (the
address moved, the width narrowed), so it serves every use a function
already had -- DISPLAY, MOVE, comparison, and as another function's
argument. A numeric function is refused; an expression for the position
or length is refused as not implemented yet (the width would be known
only at run time, which the function machinery does not carry).
free/fnrefmod, agreeing with GnuCOBOL -std=cobol85;
bad/fn-refmod-numeric.

Found with it: `DISPLAY FUNCTION REVERSE(...)` was taken for a positioned
DISPLAY, REVERSE being one of RM/COBOL's video attributes; a word after
FUNCTION is a function's name now. -std=85 byte-identical on all 227
Open Systems programs; CCVS-85 unchanged; majesty PASS.

### 55. Exception handling, part two: EC-SIZE from arithmetic (2026-09-28)

With checking on for EC-SIZE-ZERO-DIVIDE, -OVERFLOW or -TRUNCATION, an
ADD, SUBTRACT, MULTIPLY, DIVIDE or COMPUTE (CORRESPONDING included)
with no ON SIZE ERROR phrase is compiled as if it had one, and the
phrase's place raises the condition libcob saw (2023 14.7.5): a zero
divisor, an intermediate past the 64-bit, 18-digit arithmetic this
compiler uses (rule 3), or a result too large for its receiver. The
receiver keeps its value, a lone NOT ON SIZE ERROR phrase is ignored,
and all three being fatal, the run ends after the declarative. An
explicit ON SIZE ERROR still handles its own statement, and no
declarative runs (14.7.5, first paragraph).

libcob's size-error flag now says which it was (cob_size_kind).
Checking off: nothing changes. One edge, allowed by the text's
"undefined": with only some EC-SIZE names turned on, a size error of an
unchecked kind leaves the receiver unchanged instead of truncating,
since the statement is compiled in size-error mode.

EC-SIZE-EXPONENTIATION (the ** rules) and EC-SIZE in expressions outside
arithmetic statements (a condition's `a / 0`) are not raised yet.

Tests: 2002/ecsize (truncation; the unchecked and phrase-handled cases
before the TURN), 2002/eczdiv, 2002/ecovfl -- one program each, since a
fatal condition ends the run. No oracle (GnuCOBOL 4 has no exception
declaratives). -std=85 byte-identical on all 227 Open Systems programs;
CCVS-85 unchanged; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 56. Exception handling, part three: EC-BOUND-SUBSCRIPT and EC-BOUND-REF-MOD (2026-09-28)

With checking on, every subscript computed at run time -- a data item or
an index-name, with its +/- integer -- is tested against 1 and the
dimension's OCCURS maximum (2023 8.4.2.3.4 rule 2: the maximum, also
for an OCCURS DEPENDING ON table) in `emit_ref_addr`, the one place a
subscript becomes an address: one unsigned compare and branch, nothing
at all with checking off. A reference modification with a computed
leftmost position or length is tested by libcob's cob_bound_refmod
(8.4.2.4): the start within the item, the part not past its end.
Literal subscripts and positions were already compile-time errors.
EXCEPTION-STATEMENT now names the statement for every condition raised
under WITH LOCATION, not only RAISE (the compiler keeps the current
statement's name). A check is made while the statement's operands are
identified, so a DISPLAY that has already written a literal before
reaching the bad subscript has written it -- the statement is
interrupted there, as the text puts it.

Tests: 2002/ecbound (an unchecked store to elem(9) of five landing in
the next item, then elem(5) passing and an index-name at 6 raising),
2002/ecrefmod (s(8:3) and s(8:) passing, s(8:4) raising). No oracle.
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS.

Open: EC-BOUND-ODO (a DEPENDING ON value outside the OCCURS range when
the table is referenced). And a note found reading the calling
convention for these checks: registers r11-r28 are callee-saved in the
SLOW-32 C ABI, but a COBOL program's prologue saves only r11 while the
generated code uses r12 and r13 as scratch. The C that calls generated
code today never sees it: the generated main ends in cob_stop_run, which
exits rather than returning to the libc start-up that called it, and
the registration (.init_array) and CANCEL reset routines libcob calls
touch neither register. A C caller of a whole COBOL program, returning,
would lose r12/r13. (Fixed the same day: ISSUES-57.)

### 57. A COBOL program did not preserve r12 and r13 for its caller (found and fixed 2026-09-28)

Found writing ISSUES-56. The SLOW-32 C ABI makes r11-r28 callee-saved
(docs/CALLING_CONVENTION.md); a COBOL program's prologue saved r11
only, while the generated code uses r12 and r13 as scratch (the open
mode in a USE dispatch, a dynamic CALL's target, the EC-SIZE kind). A C
function holding values in them across a call to a COBOL program --
through the C bridge, COBOL calling C calling COBOL -- got them back
changed. The frame grows from 112 to 120 bytes; the prologue saves both
(SLOT_R12, SLOT_R13) and the epilogue restores them. No other
callee-saved register is used by the generated code.

free/calleesaved: a COBOL main calls C, which keeps fourteen values
live across a call into a COBOL program making a dynamic CALL, and
checks their sum -- CLOBBERED with the previous compiler, preserved
now; GnuCOBOL agrees. Every program's prologue changed, so the gates
were the paper: harness 177/177 with CCVS-85 unchanged, majesty PASS,
majesty-functions PASS, Open Systems paper unchanged in all seven
modules.

### 58. Exception handling, part four: EC-I-O from the I-O status (2026-09-28)

With checking on, an input-output statement's I-O status raises the
condition its first digit names (2023 9.1.13): 1 EC-I-O-AT-END, 2
-INVALID-KEY, 3 -PERMANENT-ERROR, 4 -LOGIC-ERROR, 5 -RECORD-OPERATION,
6 -FILE-SHARING, 7 -RECORD-CONTENT, 9 -IMP; a successful status other
than 00 raises EC-I-O-WARNING, which only its own name turns on. The
order is USE general rule 3's: the statement's AT END or INVALID KEY
phrase; the file's USE AFTER ERROR procedure; the open mode's; then
the exception declaratives, most specific first. libcob keeps every
statement's status (cob_io_class), not only the errors'.

After a fatal status the implementor chooses (9.1.13). Here: when a
USE AFTER ERROR procedure handled it, the run goes on as COBOL 85
programs expect; when an exception declarative did, or nothing did,
the run ends.

2002/ecio: AT END with and without the phrase, a duplicate alternate
key's 02 as a warning, a missing file's own USE procedure coming first,
and a missing file with none ending the run. Writing it turned up the
text's own rule working: a READ after the AT END condition was reported
is 46, a logic error, and fatal. No oracle.

Found on the way: the first cut allocated the warning label whether or
not EC-I-O-WARNING was on, which renumbered every later label in every
program -- equivalent code, but the byte-identical gate caught it (224
of 227 Open Systems programs "differed"). Allocated only when needed,
-std=85 output is byte-identical again on all 227. Harness 178/178;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 59. Exception handling, part five: EC-PROGRAM-NOT-FOUND (2026-09-28)

With checking on and no ON EXCEPTION phrase, a CALL resolves its program
at run time through the registry, as a CALL with the phrase already did,
and a program the run unit does not hold raises EC-PROGRAM-NOT-FOUND
(2023 14.9.4 general rule 3b; fatal: the declarative, then the end). A
lone NOT ON EXCEPTION phrase does not count as handling it. As with the
phrase, the link then no longer needs the program.

2002/ecpgm: a found program, a missing one taken by the ON EXCEPTION
phrase, then a missing one raising the condition. No oracle.

Not done: EC-PROGRAM-RECURSIVE-CALL through the caller's declarative.
The re-entry is detected in the called program's prologue
(cob_act_enter), which ends the run whether or not checking is on;
the condition belongs to the CALL statement, and routing it there needs
the caller to learn the callee's active state before the call. (Done the same day: ISSUES-60.)
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS.

### 60. EC-PROGRAM-RECURSIVE-CALL raised at the CALL (2026-09-28)

Under -std=2002 every program registers its activation descriptor with
libcob beside its entry (cob_register_act; a separate call, so -std=85
output is unchanged). With checking on, a CALL first asks
cob_program_busy whether the named program is active and not RECURSIVE
(2023 14.9.4 general rule 3f), and if so raises the condition there, so
the caller's declarative runs before the run ends (fatal). With
checking off, the called program's prologue still stops the run, as
2002/recnot shows. 2002/ecrecur. No oracle. -std=85 byte-identical on
all 227 Open Systems programs; harness 180/180; majesty PASS;
majesty-functions PASS.

### 61. EC-BOUND-ODO (2026-09-28)

With checking on, a reference to an OCCURS DEPENDING ON table, to an
item in it, or to a group holding it tests the DEPENDING ON value
against the OCCURS bounds before the address is formed (2023 13.18.38
general rule 7), in emit_ref_addr. 2002/ecodo: the group and an element
with N inside 2 TO 5, then the group with N at 7 raising. No oracle.

The first cut crashed ("read out of bounds at 0xffffffff"): emit_args
loads plain references straight into the argument registers, trusting
ref_needs_call that forming the address calls nothing, and the check
loads the DEPENDING ON item through libcob. ref_needs_call now says so
whenever the check will be emitted, and the reference is staged in the
frame first. (The subscript check needs no such entry: on success it
touches only r2, and its failing path never returns.) Harness 181/181;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS.

### 62. NATIONAL, part one (2026-09-28)

Stage B's NATIONAL module begins. The user ruled the two implementor
choices: a national character is a UTF-16 code unit stored big-endian
(IBM's representation), and alphanumeric text becoming national is
UTF-8 (a byte that begins no valid sequence stands for its Latin-1
character). docs/national.md has the design.

- libcob: a national descriptor category (COB_NATIONAL); MOVE into a
  national item decodes UTF-8 (or takes numeric digits, or national
  code units) and pads with national spaces, JUSTIFIED honoured;
  comparisons with a national operand compare code units, padding with
  national spaces; DISPLAY writes UTF-8; cob_fill_nat for figuratives.
- The compiler: PICTURE N (recognized before pic_analyse, which does not
  know N), USAGE NATIONAL, N"..." and NX"..." literals (a national flag on
  the token), VALUE per 13.18.63 syntax rule 5, national routing ahead
  of every MOVE fast path, figuratives and ALL literals made national
  beside a national operand, LENGTH in characters, INITIALIZE ...
  REPLACING NATIONAL.
- Refused, not silently treated as bytes: STRING, UNSTRING, INSPECT,
  ACCEPT and reference modification of national items; numeric and
  edited national; national in reports and screens. A MOVE of national
  to an elementary alphanumeric or numeric item is refused as the text
  does (use DISPLAY-OF, part two).

2002/national; refusals national-85, std2002-nat-value-alnum,
std2002-nat-to-alnum, std2002-nat-inspect, std2002-nat-refmod. No
oracle: GnuCOBOL 4's national data is unfinished (docs/national.md).
Harness 187/187; -std=85 byte-identical on all 227 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

Next (part two): NATIONAL-OF, DISPLAY-OF, CHAR-NATIONAL, whose results'
lengths are known only at run time -- the variable-length function
result machinery EXCEPTION-LOCATION, EXCEPTION-FILE, TRIM and CONCAT
also wait on.

### 63. UTF-8 source columns, and malformed UTF-8 in national conversion (2026-09-28)

Two rulings from a discussion of part one's encodings, framed by the
user as the choices of an implementation on the non-IBM side of the
fence:

- **Fixed-form columns count code points.** IBM (a column is "a byte
  position") and GnuCOBOL count bytes, as this compiler did; but a card
  image carried from EBCDIC into UTF-8 grows a byte for every accented
  letter, and its layout breaks -- a literal ending at column 72 runs
  into the sequence area. The reader now places columns 7, 8 and 73 by
  characters; `-fixed-columns=bytes` restores byte counting (compile.sh
  passes it through). ASCII sources are unaffected: -std=85 output is
  byte-identical on all 227 Open Systems programs, and CCVS-85 is
  unchanged. fixed/utf8cols (no oracle; GnuCOBOL counts bytes).
- **Invalid UTF-8 is malformed, not Latin-1.** Part one read a byte that
  begins no valid sequence as its Latin-1 character; a byte string
  cannot be both encodings. It now becomes U+FFFD, and with checking on
  a MOVE into a national item raises EC-DATA-CONVERSION (14.9.25 general
  rule 6; nonfatal). A national literal with invalid UTF-8 is a compile
  error. 2002/natconv. A first cut left libcob's conversion flag set
  from an unchecked MOVE, so a later valid one raised the condition;
  each MOVE now clears it.

Surrogates were confirmed, not changed: one PIC N position is one UTF-16
code unit, a supplementary character two (the text, 8.5.1.4, and IBM's
Language Reference agree). docs/national.md, docs/dialect.md. Harness
189/189; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

### 64. NATIONAL, part two: NATIONAL-OF, DISPLAY-OF, CHAR-NATIONAL (2026-09-28)

The three conversion functions of 2002's national module, and the first
function results whose length is known only at run time. `café` gives
four national characters from five bytes; "日本語" gives nine bytes from
three characters. The compiler knows an upper bound (2 bytes per byte
for NATIONAL-OF, 3 per character for DISPLAY-OF; a bound past 8190
bytes is refused), and the length itself comes from libcob when the
function is evaluated:

- the O_FUNC operand carries `fvar` (run-time length) and `fnat`
  (national result);
- a string argument (MOVE source, comparison, DISPLAY) takes r4 from
  `cob_fn_last_len`, and a CALL-style descriptor operand from
  `cob_fn_var_desc`, evaluated right after the function;
- FUNCTION LENGTH and BYTE-LENGTH of such a result become a run-time
  count (`cob_fn_last_len_digits`);
- `opnd_size` refuses them, so no path can silently use the bound as
  the length; reference modification of one is refused for now.

What does not convert becomes the substitution character (argument-2)
or U+FFFD. Without argument-2 and with EC-DATA-CONVERSION checked,
libcob notes the substitution and the statement raises the condition
when it completes (15.66.4 rule 3, 15.26.4 rule 3). A first cut raised
it inside the function's evaluation, in the middle of the statement's
operands, where a declarative that returns would have run with the
operands already staged. libcob's intrinsic buffers grew from 1024
bytes to 8192 for these results.

This is the machinery EXCEPTION-LOCATION and EXCEPTION-FILE (ISSUES-53)
and 2014's TRIM were waiting on. 2002/natfuncs (no oracle: GnuCOBOL 4
has no DISPLAY-OF); eight refusal fixtures. The fixture that used
CHAR-NATIONAL as an example of an unimplemented module now uses
BOOLEAN-OF-INTEGER. Harness 198/198; -std=85 output byte-identical on
all 227 Open Systems programs; majesty PASS; majesty-functions PASS;
Open Systems paper unchanged.

README ruling 5 was reworded at the same time, at the user's direction:
"UTF-8 sources. No EBCDIC on this ISA." It was always about the
machine's character set, never a refusal of non-ASCII text. It now names
the mainframe data formats that do not carry over unchanged: packed-decimal
sign nibbles are IBM's bytes, zoned DISPLAY signs are not, and
COMP-1/COMP-2 cannot mean IBM hexadecimal floating point.

### 65. EXCEPTION-LOCATION and EXCEPTION-FILE (2026-09-28)

2002 15.23-15.26, with their national forms (-N), on ISSUES-64's
run-time-length results. The last two of the exception-status functions
ISSUES-53 left open.

- **EXCEPTION-LOCATION** is "program; paragraph OF section; line", or
  "section; line" in a section with no paragraph, or "; line" with
  neither (15.25.2 rule 2b). The string is known at compile time and
  passed to `cob_ec_raise` as a literal, only when checking for the
  condition was turned on WITH LOCATION. Otherwise, and before any
  condition, the result is one space: the text leaves saving the
  location without LOCATION to the implementor, and this one does not.
  The line identifier is implementor-defined: the statement's line
  number, or `copybook:number` for a statement copied in.
- **EXCEPTION-FILE** is the I-O status and the file-name when the last
  condition is EC-I-O, and two zeros otherwise. The EC-I-O path passes
  the file-name to `cob_ec_raise`, which takes the status from the I-O
  statement's own.
- **Names as written.** Both functions return names "exactly as
  specified in the source", and the tokenizer lowercased every word, so
  a word now keeps its spelling (`Tok.orig`, set only when it differs).
  Paragraphs, sections, the program-name and SELECT's file-name carry
  it.
- **EXCEPTION-STATEMENT.** The statement's name and first token are now
  saved around a nested statement and restored after it, so a condition
  raised at the end of a statement names that statement, not the last
  one nested inside it.

2002/ecloc (no oracle); bad/excloc-85. Harness 199/199; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 66. UPPER-CASE and LOWER-CASE on national and UTF-8 text (2026-09-28)

2002 15.78 and 15.52 give a national argument a national result. Before
this, a national argument was cased as bytes and its result taken as
alphanumeric: `UPPER-CASE(N"a慢b")` gave " AA", because 慢 is U+6162,
whose bytes are "ab", and the six bytes were then read back as UTF-8.
Now:

- **The mappings.** Unicode's simple mappings, UnicodeData.txt fields 12
  and 13, as 2002 Annex D note 1 advises. They are taken from libutf's
  copy (Unicode 16.0), so the two agree. `libcob/gen_casemap.py` writes
  `libcob/casemap.h`: 205 upper and 187 lower runs of {lo, hi, stride,
  delta}, searched by bisection. The generator checks that its runs
  reproduce the mappings exactly and that no mapping crosses between the
  BMP and the supplementary planes, which the national path relies on.
- **National data.** A code unit maps as itself, and a surrogate pair as
  its character.
- **Alphanumeric data** is UTF-8 text (README ruling 5). A well-formed
  multi-byte character is mapped when its other case has the same byte
  length; with no locale the result keeps the argument's length
  (E.13.2.4). Any other byte is left as it is. ASCII behaves as before.
  GnuCOBOL cases bytes in the C locale, so it differs only on non-ASCII
  text.
- **Run-time-length arguments.** An argument whose length is known only
  at run time (UPPER-CASE of NATIONAL-OF) gives a result of the same
  run-time length.
- **Reference-modified arguments** (found on the way). The function took
  the whole item's size as the length, so `UPPER-CASE(a(2:3))` returned
  ten characters from the second byte of a ten-byte item. It now returns
  the three selected. free/casermod (the oracle agrees). A reference
  modification whose length is an expression is refused.

2002/natcase (no oracle), free/casermod. Harness 202/202; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 67. NATIONAL, part three begins: reference modification (2026-09-28)

Reference modification of a national item (2023 8.4.2.4) is the first
row of part three, the one STRING, UNSTRING and INSPECT of national data
will stand on. Start and length count character positions, and the part
is national.

- **Compiler.** A `Ref` carries `rm_nat`. Its literal start and length
  stay in characters and become bytes where they are used: the address
  offset, the static length, and the part's descriptor (`nat_desc`, not
  `str_desc`). A computed start is doubled after the bound check. The
  compile-time "past the end" checks count characters.
- **Runtime.** `cob_refmod_desc` and `cob_refmod_len` read the item's
  category: a national part is national, and its length in bytes is
  twice its characters. `cob_bound_refmod` is given the size in
  characters.
- **MOVE.** A national MOVE takes a reference-modified receiver: a
  figurative constant fills it through `cob_fill_all` with the constant's
  two bytes, and ALL and ordinary sources get the part's length and
  descriptor.

bad/std2002-nat-refmod, which was the refusal, now checks the character
count: `n(4:2)` runs past the end of a PIC N(4). 2002/natrefmod (no
oracle) covers sending and receiving parts, literal, computed and
omitted positions, a table element, comparison, and a fatal
EC-BOUND-REF-MOD.

Open, not national-specific: FUNCTION LENGTH of a reference modification
whose length is an expression is refused for alphanumeric items too.

Harness 203/203; -std=85 byte-identical on all 227 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

### 68. NATIONAL, part three: INSPECT (2026-09-28)

INSPECT of a national item (2023 14.9.22), all three forms (TALLYING,
REPLACING, CONVERTING) with BEFORE and AFTER, on a whole item or a
reference-modified one.

- **Runtime.** The INSPECT engine was byte-wise; it now takes a
  character width, 1 or 2, from the item's descriptor in
  `cob_inspect_begin`. The scan steps by characters, CHARACTERS takes
  one character, a BEFORE/AFTER search and a match succeed only at a
  character boundary, and CONVERTING pairs characters. So N"AB" (bytes
  00 41 00 42) does not contain NX"4100", which a byte scan would find
  at offset 1.
- **Compiler.** Syntax rule 4: beside a national item every operand is
  national, and beside any other item none is. Both are refused with the
  rule's number. Before this, a national literal in INSPECT of an
  alphanumeric item was compared as bytes, silently. A figurative
  constant is one national character (rule 3), including the one
  CONVERTING stretches to the length of its other operand.
- **Byte identity.** Nothing changes for an item that is not national:
  width 1 is the old code path.

2002/natinspect (no oracle). bad/std2002-nat-inspect, which was the
refusal, now checks rule 4 on a national item; bad/std2002-inspect-natop
checks it on an alphanumeric one. Harness 205/205; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 69. NATIONAL, part three: STRING and UNSTRING (2026-09-28)

STRING into a national receiver and UNSTRING of a national source (2023
14.9.43, 14.9.48).

- **Runtime.** Both statements get a character width, set by their own
  begin calls: `cob_str_begin_nat` and `cob_unstr_begin_nat`, so the
  calls a non-national statement makes are unchanged. Positions stay
  bytes inside, and the POINTER is converted on the way in and out. The
  copy, the delimiter search and ALL's repeats step by characters.
  COUNT IN is characters, and an UNSTRING part is moved as national.
- **Compiler.** The class rule both ways (rule 1 of STRING, rule 3 of
  UNSTRING), for sources, delimiters, receivers and DELIMITER IN items.
  Before this, only a national data item was refused there; a national
  literal in STRING into an alphanumeric receiver compiled and was
  copied as bytes. A figurative constant is one national character
  (`fig_char_args`). A numeric UNSTRING receiver of national data would
  have to be USAGE NATIONAL (rule 4); that is not implemented, so it is
  refused.

2002/natstring (no oracle): DELIMITED BY SIZE and by a national
literal, a figurative source, POINTER, ON OVERFLOW, UNSTRING with OR,
ALL, DELIMITER IN, COUNT IN, TALLYING IN and POINTER, and a delimiter
that is present only across a character boundary.
bad/std2002-nat-string and bad/std2002-nat-unstring-num. Harness
208/208; -std=85 byte-identical on all 227 Open Systems programs;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 70. NATIONAL, part three: ACCEPT (2026-09-28)

ACCEPT into a national item. Every form already went through `cob_move`
with alphanumeric text as the source, and a national receiver's move
decodes UTF-8 (ISSUES-62/63), so the runtime needed nothing. The
compiler stops refusing, and with EC-DATA-CONVERSION checked, it clears
the conversion flag before the ACCEPT (at end of input nothing is
moved, and a flag left by an earlier unchecked MOVE must not raise the
condition) and tests it after. ACCEPT at a screen position waits for
national screen fields and is refused by name.

This was the last statement using `g_nat_forbid`, the blanket "a
national item in X is not implemented yet" refusal; it is gone. It only
ever covered INSPECT, STRING, UNSTRING and ACCEPT, which now take
national data by their own rules.

2002/nataccept (no oracle; .keys, .args and .env): a long line and a
short one, invalid UTF-8 unchecked and checked, end of input, COMMAND-
LINE, ARGUMENT-VALUE and DATE. bad/std2002-nat-accept-at. Harness
210/210; -std=85 byte-identical on all 227 Open Systems programs;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 71. NATIONAL, part three: national groups (2026-09-28)

`GROUP-USAGE IS NATIONAL` (2023 13.18.29). A national group is treated
as an elementary national item of its length (general rule 2b), except
where the text processes it as a group.

- **One predicate.** `sym_is_national` answers for both an elementary
  PIC N item and a national group, and the national checks from
  ISSUES-62 to -70 now use it. That covers MOVE, comparison, reference
  modification, INSPECT, STRING, UNSTRING and ACCEPT. A national
  group's descriptor is national, so the runtime needed nothing.
- **The rules.** GROUP-USAGE applies to a group only (rule 1), with no
  USAGE clause of its own (rule 3). A subordinate group inherits it, and
  every elementary item under it must be national, which today means
  PICTURE N. VALUE takes a national literal, as for PIC N.
- **As a group.** INITIALIZE (14.9.20.4 rule 1) and MOVE CORRESPONDING
  (the MOVE statement's note 5) keep processing it as a group, and an
  alphanumeric group receives its bytes (14.9.25 rule 4).
- **GROUP-USAGE BIT** is refused by name: it is the BOOLEAN module.

Found on the way: when a data entry that turned out to be a group was
refused, the FILLER PIC X put in its place (the recovery from ISSUES-41)
then drew a second error, "'filler' is a group and cannot have a
PICTURE". The stand-in is marked now and drops its PICTURE quietly.

2002/natgroup (no oracle). Refusals: std2002-natgroup-alnum, -elem,
-usage, std2002-groupusage-bit, groupusage-85. Harness 216/216; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 72. NATIONAL, part three: numeric and numeric-edited USAGE NATIONAL (2026-09-28)

A numeric or numeric-edited picture with USAGE NATIONAL (2023 13.18.60
rule 12), explicit, from a group's USAGE, or implied by a national group.

- **Representation** (the implementor's choice, recorded in
  docs/national.md): the DISPLAY form with each character one UTF-16BE
  code unit. The text specifies a separate sign's characters and leaves
  an unseparated sign to the implementor; here it is the DISPLAY
  overpunch widened. So -126 in PIC S9(4) is 00 30 00 31 00 32 00 76.
- **Runtime.** A new usage, `COB_U_NATIONAL`. Each numeric primitive
  (`cob_get_num`, `cob_put_num_x`, `num_to_digits`, `cob_move`,
  `cob_cmp`, `cob_class`, `cob_display_field`) narrows such an operand
  to a scratch DISPLAY copy and widens a receiver back afterwards.
  Arithmetic, comparison, editing and DISPLAY all come through
  unchanged. `cob_move` also takes a national sender to a numeric
  receiver, as its UTF-8.
- **Compiler.** `U_NATIONAL`, sized at twice the DISPLAY size. Every
  compiler path that tests a usage was audited: `is_hot_int` excluded
  only DISPLAY and PACKED and would have taken U_NATIONAL for binary.
  The DISPLAY-only fast paths (`is_display_int`, `sym_dec_ok`, the
  inline decimal add) test for U_DISPLAY, and a national item takes the
  runtime path. Initial values are made as DISPLAY and widened; a
  nonnumeric VALUE is a national literal (13.18.63 rule 5). LENGTH
  counts characters, and INSPECT scans them. UNSTRING takes a numeric
  national receiver, as rule 4 allows.
- **The MOVE table (14.9.25) by category.** National to numeric or
  numeric-edited is valid, and was refused until now; numeric noninteger
  to national is not, and was accepted until now.
- **Refusals.** A picture of A or X with USAGE NATIONAL (rule 12). A
  signed numeric item in a national group without SIGN SEPARATE
  (13.18.29.3 rule 3). National-edited pictures (N with B, 0 or /),
  which got an unhelpful "not valid at character 1", are now refused by
  name.

A slip on the way, caught by the first test run: the edit that added
`case U_NATIONAL` in `sym_finish` replaced `case U_DISPLAY` instead of
joining it, so every DISPLAY item had no size.

2002/natnum (no oracle): arithmetic with a separate and an unseparated
sign, the stored bytes, editing, comparison, MOVE both ways with PIC N
and alphanumeric, LENGTH, INSPECT, a class test, a group USAGE
NATIONAL, a national group with numeric items, and UNSTRING into a
numeric national receiver. Refusals: std2002-nat-noninteger,
-natgroup-sign, -nat-usage-x, -nat-edited; natgroup-alnum's message
follows rule 12; nat-unstring-num now refuses only a DISPLAY numeric
receiver. Harness 221/221; -std=85 byte-identical on all 227 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 73. NATIONAL, part three: national-edited pictures (2026-09-28)

PICTURE N with B, 0 and / (2023 13.18.40).

- **Compiler.** `nat_picture` takes the insertion symbols and flattens
  the picture into one symbol per character position; `pi.edited` marks
  it, and the descriptor carries the pattern as it does for
  alphanumeric-edited. The category stays national, so every national
  path (comparison, DISPLAY, INSPECT, reference modification, sending)
  takes it without change. A figurative constant or ALL literal moved to
  one becomes a national literal of the item's length and goes through
  the editing move. An UNSTRING receiver may not be national-edited
  (rule 4); STRING already refused edited receivers.
- **Runtime.** `move_to_national` edits when the receiver's descriptor
  has a pattern: the sending characters fill the N positions, and B, 0
  and / insert U+0020, U+0030 and U+002F.

A slip on the way: the first splice of the new `nat_picture` was bounded
by the first occurrence of `parse_data_item1`, which is its forward
declaration, so the edit duplicated code and the build failed. The file
was restored from HEAD, which had nothing else pending, and the edit
redone bounded by the function's own closing brace.

2002/natedit (no oracle): a date picture, space and zero insertion, an
integer sender, SPACES and ALL, the item as a sender, VALUE, LENGTH,
INSPECT. bad/std2002-nat-edited, which was the refusal, now checks the
UNSTRING rule. Harness 222/222; -std=85 byte-identical on all 227 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 74. NATIONAL, part three: national records in files (2026-09-28)

- **Record sequential, relative, indexed.** Nothing to do: records are
  bytes, so a national record goes out as UTF-16BE, and a national
  RECORD KEY orders by code unit, which is its byte order. The test
  shows both.
- **Line sequential: UTF-8 text** (my call; the text leaves the
  character set to the implementor, 2023 12.4.5.10 general rule 2, and
  anticipates national records, 14.9.30 rule 15 and 14.9.51 rules
  21-23). The alternative, UTF-16 bytes in a text file, breaks on a 0A
  byte inside a character. WRITE encodes the record with trailing
  national spaces dropped, and fails with 71 on a lone surrogate. READ
  decodes and pads with national spaces: 09 when a byte is not UTF-8
  (U+FFFD), 04 when the line holds more characters than the record.
- **How.** `ls_read_national` and `ls_write_national` run the ordinary
  line sequential code once over a UTF-8 buffer, so the printer and
  LINAGE paths, CRLF and ADVANCING come along unchanged. The compiler
  marks such a file with `varying = 2`, a field only record sequential
  files read, rather than adding a field to `cob_file`: that would
  change the image of every file in every program, and the -std=85
  byte-identity gate with it. A file mixing national and alphanumeric
  records is refused.

A note from the test: DISPLAY of a record holding X'FF' writes that
byte, and macOS sed stops at it ("illegal byte sequence"); the test
reports that line without displaying the byte.

2002/natfiles (no oracle): line sequential written and read both as
national and through an alphanumeric view of the same file, statuses
09, 04 and 71, record sequential bytes, an indexed national key in
code-unit order and by key. bad/std2002-nat-ls-mixed. Harness 224/224;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 75. NATIONAL: the module closed, with Report Writer and screens refused (2026-09-28)

(The refusals below were lifted by ISSUES-92.)

With ISSUES-62 to -74, every category of national data in the text works:
national, national-edited, numeric and numeric-edited USAGE NATIONAL,
and national groups. So do every statement that takes them and national
records in files.

What remains is placing national text in character cells: a report
line's COLUMN, a screen field's LINE and COL. An East Asian character
takes two cells on a terminal or a page, so "column" needs a ruling
(code units, code points, or display width; libutf's console_width.c
has the third). Both modules are optional since 2014 (its E.2 item 23).
So they are refused by name, not built:

- a PIC N report field and a PIC N screen field. These got "PICTURE
  'n(4)' is not valid at character 1", which named no reason.
- a national SOURCE for a report field, and a national FROM/TO/USING
  item for a screen field. Both **compiled silently**, and would have
  copied UTF-16 bytes into an alphanumeric field.
- positioned ACCEPT of a national item (ISSUES-70).

bad/std2002-nat-rw, -nat-rw-source, -nat-screen, -nat-screen-from.
Harness 228/228; -std=85 byte-identical on all 227 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

### 76. BOOLEAN, part one: boolean data in DISPLAY and NATIONAL usage (2026-09-28)

The next Stage B module after NATIONAL. Part one is boolean data and
everything that moves or tests it; expressions and USAGE BIT follow.
docs/boolean.md has the whole of it.

- **Data.** PICTURE 1 (a new category, PIC_BOOLEAN; COB_BOOLEAN in
  libcob). USAGE DISPLAY stores a character 0 or 1 a position. USAGE
  NATIONAL stores the same widened, through the ISSUES-72 narrowing,
  which needed only to admit the category. USAGE BIT is refused by
  name.
- **Literals.** `B"..."` and `BX"..."` are tokens carrying `boolv`, held
  as characters 0 and 1.
- **MOVE** by the 14.9.25 table, in `emit_move_boolean`, ahead of the
  national path, which would otherwise take a national-to-boolean MOVE
  for a forbidden national-to-alphanumeric one. The runtime aligns left
  and zero-fills (14.6.8.6). ZERO and ALL B"..." expand to boolean
  literals; any other figurative is refused (rule 7).
- **Conditions.** Boolean against boolean only, zero-extended on the
  right (8.8.4.2.8). The simple boolean condition (8.8.4.3) for one
  position, and the BOOLEAN class test (8.8.4.4), runtime class 4.
- **Also.** INITIALIZE zeros, REPLACING BOOLEAN DATA BY; reference
  modification, whose part is boolean; LENGTH in positions.
- **Functions.** BOOLEAN-OF-INTEGER and INTEGER-OF-BOOLEAN. With a
  length item, BOOLEAN-OF-INTEGER's result has a run-time length
  (`cob_fn_var_desc` kind 2).

Reference modification of any USAGE NATIONAL numeric or boolean item is
refused for now. Before this, numeric national reference modification
counted bytes, not characters; it came in with ISSUES-72 and was never
tested.

Oracle: none. GnuCOBOL 4, measured, rejects 2002/boolean: no simple
boolean condition, no BOOLEAN class test, no INITIALIZE REPLACING
BOOLEAN, no MOVE ALL B"10". std2002-intrinsic-module now uses
LOCALE-COMPARE, as BOOLEAN-OF-INTEGER exists; two rule-12 messages name
boolean.

2002/boolean (no oracle). Refusals: std2002-bool-from-num, -bool-to-num,
-bool-cmp-alnum, -bool-cond-wide, -bool-value-alnum, -bool-space,
-usage-bit, -bool-literal-digit, boolean-85. Harness 238/238; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 77. BOOLEAN, part two: boolean expressions (2026-09-28)

B-NOT, B-AND, B-XOR, B-OR and the four shifts (2023 8.8.2), in COMPUTE
format 2 (14.9.8) and in conditions.

- **Parsing** is a shunting yard that emits as it resolves, like
  `parse_expr`: operands are pushed, operators applied in postfix order.
  A shift's precedence is the preceding operation's at its level of
  parentheses, B-AND's when there is none (rule 7b), and it carries its
  integer count.
- **Runtime.** A stack of boolean values, as strings of 0 and 1:
  `cob_bpush` (narrowing USAGE NATIONAL operands), `cob_bnot`,
  `cob_band`/`bor`/`bxor` (the shorter operand zero-extended, rule 9),
  `cob_bshift` (the operand's length kept, rule 8), `cob_bstore` (by the
  MOVE rules), `cob_bcmp`.
- **COMPUTE** takes boolean receivers, not mixed with numeric ones;
  `parse_ref_list` gets a mode that admits them for COMPUTE alone.
- **Conditions.** A boolean expression is an O_BEXPR operand, re-parsed
  at emission as arithmetic expressions are. Alone, it is a simple
  boolean condition when all its operands are one position.

A regression on the way: the first cut peeked at every condition
operand for a following boolean operator, ahead of the unary-sign check,
so a condition beginning with `-` died in `parse_operand`. free/unaryexpr
and one CCVS program failed. The peek now runs only under -std=2002, at
a word or literal. The dangling-operator refusal also named the next
statement's verb ("'stop' is not declared"), and now names the rule.

2002/boolexpr (no oracle): Table A.2 row by row, the Annex D.10
examples, precedence and parentheses, the shift-precedence rule, unequal
lengths, two receivers, a USAGE NATIONAL operand, and expressions in
conditions. Refusals: std2002-bexpr-numeric, -bexpr-mixed,
-bexpr-shift, -bexpr-dangling, -bexpr-cond-wide. Harness 244/244;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 78. BOOLEAN, part three: USAGE BIT and GROUP-USAGE BIT (2026-09-28)

The first data in this compiler that is not byte-addressed.

- **Layout** (8.5.1.6.3). `layout()` carries a bit cursor: a bit item or
  bit group after one at the same level takes the next bit position;
  anything else, or the first bit item after a non-bit, starts on the
  next byte. A bit item's `bitoff` is its first bit's place in its first
  byte, and its `size` the bytes it spans. A bit group lays its
  subordinates out from its own first bit, and its `bits` is their sum.
  Items that are not bits lay out exactly as before: -std=85 output is
  byte-identical.
- **Descriptor.** `COB_U_BIT`, with `size` the bits and `scale` the
  first bit's place. libcob's USAGE NATIONAL narrowing (ISSUES-72)
  gains a BIT branch: read the bits as characters 0 and 1, and write
  back only the item's own bits. So MOVE, comparison, DISPLAY, the
  class test, the boolean stack and both functions take bits unchanged.
  `nat_widen` now takes the descriptor.
- **Compiler.** `USAGE BIT` on a boolean picture, `GROUP-USAGE BIT`
  (rules 1 and 2, inheritance as for national groups). Initial values
  are made in the DISPLAY form and packed into the record image. A bit
  sender to a group goes through `cob_move`, so the group gets the
  characters, not the packed bytes. LENGTH counts bits.
- **Refused by name:** OCCURS, REDEFINES, SYNCHRONIZED and reference
  modification of a bit item or bit group; VALUE on a bit group; bit
  items in INSPECT, STRING and UNSTRING.

2002/boolbit (no oracle): a record packing 1, 3 and 6 bits after a byte
and before a byte, checked byte by byte (61 BA 80 7A); a MOVE to one bit
item leaving its neighbours' bits; an expression, the condition, the
functions, alphanumeric in and out; a bit group and its parts.
std2002-usage-bit and std2002-groupusage-bit, which were the refusals,
now check OCCURS and the bit group's rule 2. New: std2002-bit-pic-x,
-bit-refmod, -bit-inspect. Harness 248/248; -std=85 byte-identical on
all 227 Open Systems programs; majesty PASS; majesty-functions PASS;
Open Systems paper unchanged.

### 79. TYPEDEF and TYPE (2026-09-28)

2023 13.18.58 and 13.18.57 format 1. The TYPE clause is defined by
substitution, "as though the data description identified by type-name-1
had been coded in place of the TYPE clause". `expand_types()` does that
over the tokens after COPY/REPLACE and before parsing:

- It records a TYPEDEF entry (01 or 77) and its subordinates and drops
  them, since a type has no storage.
- It replaces `TYPE [TO] name` with the type's clauses, and puts the
  type's subordinate entries after the entry, levels rebased (rule 2b),
  88s included.
- A type may use an earlier type, and the entry's own clauses (OCCURS,
  VALUE) stay.

The `>>TURN` directives' token positions are remapped. The pass runs
only under -std=2002, and only when the source says TYPEDEF or TYPE TO;
-std=85 names the switch instead.

**Why tokens, and why before parsing.** Splicing tokens into g_tok
mid-parse would move tokens that symbols already point into (a VALUE
clause's token, for one).

**Refused:** TYPEDEF STRONG (strongly-typed groups), a TYPE TO name not
declared before the entry, a group type for a level 77 item (rule 7),
and an expansion past level 49.

The test is 2002/typedecl, not typedef: GnuCOBOL refuses a source whose
base name is a C keyword. It is no oracle anyway, measured: no TYPEDEF
under -std=cobol2002, and under its default dialect, no type inside a
type. Refusals: std2002-typedef-strong, -type-unknown, -type-77-group,
typedef-85. Harness 253/253; -std=85 byte-identical on all 227 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 80. TYPEDEF STRONG: strongly-typed groups (2026-09-28)

The second half of the TYPEDEF module, and the ruling on VALIDATE, the
last row of Stage B's order.

- **Keys through the expansion.** `expand_types()` puts a marker token
  (a word no source can spell, with the key in `Tok.strong`) in each
  entry described by a strong type, and in each group inside one,
  keyed type#n. The parser sets `Sym.strong`. The same key is the same
  type.
- **Rules.** MOVE only between groups of one strong type. Comparison
  only between groups of one strong type, element by element in order:
  `cob_cmp_struct` walks a table of (offset, descriptor) pairs that the
  compiler lays out, OCCURS expanded (8.8.4.2.12). A strong type only at
  level 01 or inside a strong type (13.18.57.3 rule 6). Refused on a
  strongly-typed group: REDEFINES either way, RENAMES, reference
  modification (and of a numeric or edited item inside one), a class
  condition, INSPECT, and a STRING receiver.
- **Not checked yet:** CALL arguments of a strong type, and FD/SD
  records.

**VALIDATE** is not built, by ruling. 2023 marks it obsolete, and its
Annex E records that no COBOL provider has implemented it and neither
users nor implementors asked (docs/standards.md). It was not refused
by name, only as "not a COBOL verb"; under -std=2002 it is now refused
with the reason (std2002-validate).

2002/strongtype (no oracle): same-type MOVE and comparison, a strong
type inside a strong type, INITIALIZE, and a comparison that bytes would
get backwards (-1.00 is 0010p, 0.50 is 00050). std2002-typedef-strong,
which was the refusal, now checks rule 1. New: std2002-strong-move-other,
-strong-move-group, -strong-compare, -strong-level, -strong-redefines,
-strong-refmod, std2002-validate. Harness 261/261; -std=85 byte-identical on all 227 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 81. FUNCTION LENGTH of a computed reference modification; integer results display as values (2026-09-28)

The first of Stage B's gaps (the user's choice after Stage B closed:
close what the modules refuse by name before anything new).

- **LENGTH of `a(s:l)`** with a computed start or length was refused
  for every item (ISSUES-67 noted it). It is now counted when the
  statement runs, `FN_RMLEN`: the part's bytes from `cob_refmod_len`, in
  characters for a national item. GnuCOBOL returns the whole item's
  length, 10 for all four cases in free/lenrefmod; that is a documented
  divergence (lenrefmod.oracle-expected, docs/oracles.md).
- **How an integer result displays.** A compile-time LENGTH is a literal
  and displays as its value (`10`), which is also how GnuCOBOL shows an
  integer function. The run-time integer results -- this one, LENGTH
  and BYTE-LENGTH of a run-time-length function (ISSUES-64), and
  INTEGER-OF-BOOLEAN (ISSUES-76) -- displayed all their digits
  (`000000004`). They now carry `COB_F_INTFN`, and DISPLAY shows the
  value. The text leaves the form to the implementor. Six 2002 tests'
  expected output changed by exactly that, and nothing else did. The
  1989 date functions keep the fixed widths GnuCOBOL shows for them.

A slip while regenerating those files: in zsh an unquoted `$fl` does
not split, so a loop compiled with "-free -std=2002" as one argument,
failed, and wrote six empty .expected files. They were restored from
git before anything else, and regenerated under bash with a guard
against writing an empty capture.

free/lenrefmod (oracle, documented divergence); 2002/natrefmod gains a
computed national length. Harness 262/262; -std=85 byte-identical on all
227 Open Systems programs; majesty PASS; majesty-functions PASS; Open
Systems paper unchanged.

### 82. Reference modification of USAGE NATIONAL and USAGE BIT items (2026-09-28)

Refused since ISSUES-76 and -78. 2023 8.4.3.3.4 says what the part is
(rule 6): it keeps the item's class, category and usage, except that a
numeric or numeric-edited item gives a national part under USAGE
NATIONAL, an alphanumeric one otherwise. Positions are characters, or
bits under USAGE BIT (rule 5a).

- **USAGE NATIONAL.** Positions count characters (`rm_nat`). A numeric
  item's part is national, both sending and receiving:
  `opnd_is_national` and the national MOVE path now say so. A boolean
  item's part is boolean in national usage (`part_desc`). The runtime's
  `cob_refmod_desc` and `cob_refmod_len` follow for computed positions.
- **USAGE BIT and bit groups.** Positions count bits (`rm_bit`). The
  part's address is the byte holding its first bit, and its descriptor
  a BIT one, whose scale is that bit's place. So a part may start inside
  a byte and cross into the next. Literal positions only: a computed one
  would need the address and the bit offset computed together at run
  time, and is refused by name.

**Found on the way: majesty-functions flaked by the clock.** jerm dates
its 400,001 lines around today. The oracle's container runs in UTC and
ours ran in local time, so from 18:00 to midnight MDT the two disagreed
by a day ("FAIL jerm: output differs", the first line one day apart).
Not this change: rerun with our side in UTC, it is byte-identical. The
script now runs ours with TZ=UTC.

2002/refmodusage (no oracle): a USAGE NATIONAL numeric part sending,
receiving and computed, with LENGTH; a USAGE NATIONAL boolean part
sending, receiving and as a condition; bit parts reading, receiving,
checked in the record's bytes, and one crossing a byte.
std2002-bit-refmod now checks the computed position. Harness 263/263;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS (with the fix); Open Systems paper unchanged.

### 83. ALL literals in boolean expressions; a boolean compared with ALL took one position (2026-09-28)

- **ALL B"..." in an expression** (2023 8.8.2) was refused. It is now
  pushed with an ALL mark (`cob_bpush_all`), and a binary operation or a
  comparison repeats it to its partner's length first. The compiler
  simulates which stack entries are ALL as it emits, so rule 4 (not both
  operands), rule 5 (not a shift's first operand) and 14.9.8.3 rule 3 (a
  COMPUTE is not ALL alone) are refused at compile time. B-NOT of ALL,
  whose length the text leaves to its surroundings, is refused.
- **A bug from ISSUES-76:** `IF h = ALL B"10"` expanded the ALL literal
  to one position, and the comparison's zero padding then made
  101010 unequal to it. `cond_rel` now repeats ALL to the other
  operand's length; ZERO stays one zero, which the padding extends.

2002/boolexpr and 2002/boolean gain the cases (four new lines, nothing
else changed). Refusals: std2002-bexpr-all-both, -bexpr-all-shift,
-bexpr-all-alone. Harness 266/266; -std=85 byte-identical on all 227
Open Systems programs; majesty PASS; majesty-functions PASS; Open
Systems paper unchanged.

### 84. OCCURS on USAGE BIT items; bits at computed positions (2026-09-28)

Both need a bit address computed at run time, so they are one row.

- **A bit array** (8.5.1.6.3): occurrences follow one another bit by bit.
  `bit_total` (bits x occurrences) drives the layout and the bytes
  spanned; the array's own dimension has no byte stride (`set_dims`).
  VALUE is laid down per occurrence at successive bit offsets
  (`init_instance`).
- **A subscripted element** is a bit reference modification of the
  array: `parse_ref` sets `rm_bit`, `rm_len` = bits, and `rm_start` =
  (i - 1) x bits + 1 for a literal subscript, or `bitsub` for an item.
- **A computed start**, from a subscript or a reference modification
  expression, gets its address at run time: the byte holding bitoff +
  start - 1, as `srai 3`. Its descriptor comes from `cob_refmod_desc`,
  which for a BIT base returns a BIT part whose scale is (base scale +
  start - 1) mod 8. A bit array's element is cut from a descriptor of
  the whole array (`bitarray_desc`), so the runtime's bounds check sees
  all its bits. Address and descriptor come from the same start, so
  they cannot disagree.
- **LENGTH** of an element or a part is its bits.

**Refused by name:** OCCURS DEPENDING ON and INDEXED BY on a bit array,
OCCURS on a bit group, reference modification of an element.
std2002-usage-bit and std2002-bit-refmod, whose cases now work, check
INDEXED BY and element reference modification.

2002/bitarray (no oracle): twelve 1-bit flags with VALUE on each (61 FF
F0 7A), one cleared (61 DF F0 7A), read back by a computed subscript; 3-
bit elements across bytes from BOOLEAN-OF-INTEGER; a computed
subscript; LENGTH of an element; a computed reference modification
across a byte, and to the end. Harness 267/267; -std=85 byte-identical
on all 227 Open Systems programs; majesty PASS; majesty-functions PASS;
Open Systems paper unchanged.

### 85. REDEFINES with bit items (2026-09-28)

2023 13.18.44.4 rule 1: storage association "starts at the first bit of
the data item referenced by data-name-2". In `layout()`'s REDEFINES
branch:

- A bit item or bit group redefining takes the redefined item's bit
  offset, or bit 0 over a character item.
- A character item may redefine a bit item that starts a byte.
- One over a bit item that starts inside a byte would begin at a bit,
  which this layout cannot express. It is refused as not implemented,
  citing the rule, and not as an error in the program.
- REDEFINES never moves the bit cursor.

On the way: a redefining bit array's end was computed as its size times
its occurrences, but a bit item's size already spans its occurrences.

2002/bitredef (no oracle): bit views of a character (read, then written
through), a bit view of a bit item mid-byte leaving its neighbour
alone, a character view of a byte-aligned bit item, and sixteen flags
over two bytes. bad/std2002-bit-redef-byte. Harness 269/269; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 86. Bit items in INSPECT, STRING and UNSTRING are the standard's refusal, not a gap (2026-09-28)

ISSUES-78 refused USAGE BIT items in INSPECT, STRING and UNSTRING as
"not implemented yet", as if they were a gap. The text rules them out:
all three take items of usage display or national (14.9.22.3 rules 1-2,
14.9.43.3 rule 1, 14.9.48.3 rules 2 and 4). The message now cites the
rule, and the gap list loses the entry. std2002-bit-inspect's expected
message follows; std2002-bit-string and -bit-unstring are new. Harness
271/271; -std=85 byte-identical on all 227 Open Systems programs;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 87. >>TURN for one file (2026-09-28)

2023 7.3.25: `>>TURN exception-name [file-name] ... CHECKING ON | OFF`.
Refused since ISSUES-53.

- **What a file-name means.** A file-name follows an EC-I-O
  exception-name (rule 4) and turns its checking on or off for that
  file alone (rules 6 and 8). A TURN without a file-name sets the
  condition for every file and clears its per-file settings, since it
  applies "for all procedure division statements that follow".
- **How it is held.** Per (condition, file) overrides (`EcFile`), over
  `g_ec_on`. The code emitted after an I-O statement asks
  `ec_on_io(name, file)` for its own file, and WITH LOCATION comes from
  the override when one applies.
- **Lexical, like any TURN.** A TURN holds for the statements that follow
  it in the source (rule 6), not for the statements executed after it.
  The test's first draft PERFORMed one shared paragraph after three
  TURNs and saw the last state in every phase. That was correct, and the
  test was rewritten, not the compiler.

Noted for the gap list: reference modification of a function result
whose length is known only at run time (`FUNCTION EXCEPTION-FILE(3:8)`)
is refused. The test moves the result to an item first.

2002/ecturnfile (no oracle): on for one file; on for all and off for
one; WITH LOCATION for one. std2002-turn-file, which was the refusal,
now checks rule 4. Harness 272/272; -std=85 byte-identical on all 227
Open Systems programs; majesty PASS; majesty-functions PASS; Open
Systems paper unchanged.

### 88. Reference modification of function results: run-time lengths, and national positions (2026-09-28)

Found in ISSUES-87: `FUNCTION EXCEPTION-FILE(3:8)` was refused, as was
reference modification of any result whose length is known only at run
time.

- **A result of run-time length** (NATIONAL-OF, DISPLAY-OF,
  EXCEPTION-FILE/-LOCATION, BOOLEAN-OF-INTEGER with an item length). The
  start is checked against the compile-time maximum.
  - With a length written, the part is fixed. With EC-BOUND-REF-MOD
    checked, its end is tested against the length the result came out
    with: the national form of "日本" in PIC X(10) is six characters,
    not the ten its bound allows.
  - To its end, the part stays of run-time length:
    `cob_fn_var_skip` advances the pointer and shortens the length
    libcob recorded, so LENGTH, a MOVE and DISPLAY see the part.
- **A latent bug, national results.** Positions in a function's
  reference modification counted bytes, so a national result's `(1:1)`
  took half a character: `FUNCTION CHAR-NATIONAL(12354)(1:1)` displayed
  nothing and had LENGTH 0 at HEAD (checked). Positions now count
  characters, two bytes each in a national result (8.4.3.3.4 rule 1).

Still refused: a computed position in a function's reference
modification. std2002-natof-refmod, whose case now works, checks that.

2002/fnvarrm (no oracle): parts of NATIONAL-OF, DISPLAY-OF,
CHAR-NATIONAL and BOOLEAN-OF-INTEGER; a part to the end and its LENGTH;
a fixed part within the result and one past it (fatal). Harness 273/273;
-std=85 byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 89. The exception-checking PERFORM (2026-09-28)

2023 14.9.28 format 3: `PERFORM [WITH LOCATION] imperative-statement-1
{WHEN EXCEPTION names imperative-statement-2}... [WHEN OTHER EXCEPTION
...] [WHEN COMMON EXCEPTION ...] [FINALLY ...] END-PERFORM`.

ISSUES-53 deferred this because its range looked dynamic, which would
conflict with this compiler's compile-time exception model. The text
turns out to define its checking lexically: general rule 14 is an
implicit TURN for the WHEN names "before the first statement in
imperative-statement-1", and an implicit PUSH ALL / TURN OFF ALL / POP
ALL around the phrases. So it fits the model.

- **Recognizing it.** A pre-scan (`perform_is_ecp`) finds WHEN ...
  EXCEPTION or FINALLY at the PERFORM's own level, counting nested
  inline PERFORMs by the same test `parse_perform` uses. It is not a
  speculative parse: that would apply the >>TURNs inside twice.
- **Dispatch.** While imperative-statement-1 is compiled, the PERFORM is
  on a compile-time stack. `emit_ec_dispatch` asks it first
  (`ecp_dispatch`): the name, its group, EC-ALL, by the USE rules
  (rule 17), FILE matching the raising file; else WHEN OTHER (18). A
  match stores where to resume and whether the condition was fatal in
  two data words, and jumps; no USE declarative runs.
- **Returning** (rule 20). imperative-statement-1 is compiled a
  statement at a time, each with a resume label after it. A WHEN
  phrase's end goes on to WHEN COMMON, and the last phrase returns: a
  fatal condition ends the run, a nonfatal one jumps back to after the
  statement it arose in. WHEN OTHER resumes at the end of the PERFORM,
  which is FINALLY (16, 18).
- **State.** Checking is off in the phrases (rule 14), and a RAISE there
  is refused (14.9.29.3 rule 4). After END-PERFORM the names turned on
  for the PERFORM are off again unless they were on before (22), and
  per-file settings are restored.

**Limits, documented:**
- A condition raised in code outside the PERFORM's text -- a paragraph
  PERFORMed from inside it, with checking turned on there -- goes to
  USE declaratives. Rule 17 says "during the execution of
  imperative-statement-1", which reaches it; that would need a
  run-time handler stack.
- The resume words are static, so a RECURSIVE program raising inside a
  nested activation of the same PERFORM would share them.
- A per-file TURN made inside imperative-statement-1 is not retained
  past END-PERFORM.
- WHEN EXCEPTION with a bare file-name or an open mode is refused.
- EXIT PERFORM does not exist here at all.

2002/ecperform (no oracle): a nonfatal RAISE resumed with its USE
declarative bypassed, WITH LOCATION, WHEN OTHER and WHEN COMMON,
FINALLY, the enablement gone after END-PERFORM and a USE running once
TURNed on outside, a fatal EC-BOUND-SUBSCRIPT's phrase then the end of
the run. Refusals: std2002-ecp-raise, -ecp-file-io, -ecp-filename.
Harness 277/277; -std=85 byte-identical on all 227 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

### 90. EXIT PERFORM [CYCLE], EXIT PARAGRAPH, EXIT SECTION, PERFORM UNTIL EXIT (2026-09-28)

2023 14.9.14 formats 3 and 4, and 14.9.28 general rule 11. All four were
refused as "not in COBOL 85", under -std=2002 too.

- **EXIT PERFORM** leaves the innermost inline PERFORM, and with CYCLE
  ends its current pass (rule 5). The inline PERFORMs being compiled are
  a stack of (exit, cycle) labels: a body's cycle label follows its last
  statement, and its exit label follows the loop.
- **In an exception-checking PERFORM** (ISSUES-89), EXIT PERFORM goes to
  FINALLY or END-PERFORM, and in FINALLY past END-PERFORM (rule 4, and
  14.9.28 rule 16). CYCLE is refused there (rule 8).
- **EXIT PARAGRAPH and EXIT SECTION** go to the end of the current
  paragraph or section, before its return (rules 6, 7), so a PERFORM of
  it returns. The label is made when an EXIT asks for it, and emitted
  before the exit check at every place a paragraph or section closes.
- **PERFORM UNTIL EXIT** loops until something leaves it (14.9.28.4 rule
  11).

-std=85 output is unchanged: the labels exist only under -std=2002, since
new labels renumber every later label.

On the way: the exception-checking PERFORM's pre-scans counted the
PERFORM of EXIT PERFORM as a nested inline PERFORM, and so missed the
WHEN phrases; a PERFORM after EXIT opens nothing now.

2002/exitperform (no oracle): CYCLE and EXIT in a VARYING loop, EXIT
from an inner loop, UNTIL EXIT, EXIT PARAGRAPH and EXIT SECTION through
PERFORM, EXIT PERFORM to FINALLY. Refusals:
std2002-exit-perform-outside, -exit-cycle-ecp, -exit-section-nosec,
exit-perform-85. Harness 282/282; -std=85 byte-identical on all 227 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 91. Computed positions in a function's reference modification (2026-09-28)

`FUNCTION f(...)(k:l)`, with a start or length that is an expression,
was refused (ISSUES-88 lifted only the literal forms). The function is
evaluated at its full length. Then the start and length are evaluated,
and `cob_fn_rm` finds the part: its address, and its length recorded as
a run-time-length result's, so LENGTH, MOVE and DISPLAY see it. The
whole is the runtime's length for a run-time-length result, or the
function's fixed size, and positions count characters of the result's
unit (two bytes national). A part outside the whole is clamped and
noted, and with EC-BOUND-REF-MOD checked it raises the condition. This
applies under -std=85 too, where the 1989 functions can be
reference-modified.

The rule-2 refusal for a numeric function now cites 8.4.3.3.3 rule 2.
std2002-natof-refmod, whose case now works, is removed rather than
repurposed: fn-refmod-numeric already checks the one refusal left.

**Noted, the terminal** (asked by the user). The term service is not
Unicode-aware: `term_cell_t` holds one byte (`uint8_t ch`), the cursor
advances one column per byte, screen save/restore and the buffered
update's diff replay cells byte by byte, and term_getkey delivers a
typed character's UTF-8 bytes as separate keys. Two copies:
tools/emulator/mmio_ring.c (slow32, slow32-fast, the DBT) and
qemu-backend/target/slow32/mmio.c. Plain DISPLAY is unaffected, since
its bytes go straight to the host terminal. National fields in SCREEN
SECTION wait for a Unicode-aware term service. Report Writer does not:
it waits for the ruling on what a column is (code points or display
width).

2002/fnvarrm gains computed starts, an expression start, a start to the
end with LENGTH, and a computed length. Harness 281/281; -std=85
byte-identical on all 227 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 92. National fields in Report Writer and SCREEN SECTION, by display width (2026-09-28)

This closes the refusals of ISSUES-75. The ruling on what a column is:
display width ("visual width is the right answer -- albeit not a
perfect one"). The term service became Unicode-aware for it (4dfb334c),
with the same width table, `common/term_width.h`. docs/national.md
"National text in columns" has the rule. In short, a field of n
national positions is n columns, its text laid out by width, and a
character that would cross the last column is dropped. Positions,
LENGTH and reference modification still count code units.

- **Report Writer.** `cob_rw_field` lays a national field out as
  clusters (`nat_clusters`). The line keeps a byte per column for
  alphanumeric fields plus a UTF-8 cell per column for national ones
  (`rw_kind`, `rw_u8`), and a double-width character's second column is
  marked. Overwriting half of one blanks the other half. The compiler
  takes PIC N and national-edited fields, USAGE NATIONAL on a numeric
  or numeric-edited PICTURE, and a national VALUE (a no-PICTURE one as
  wide as it shows). A field with no COLUMN follows the one before by
  its columns.
- **Screens.** A national slot has a national picture descriptor and a
  width in columns. It is painted from clusters (`scr_paint_nat`: SECURE
  gives an asterisk a column, PROMPT fills spaces) and edited as a
  cluster list. `scr_key` decodes UTF-8, and the special keys moved to
  0x110001 on. Positioned DISPLAY of a national item or literal and
  positioned ACCEPT of a national item take the same path. The first
  was quietly wrong before: a national item's UTF-16 went through an
  alphanumeric slot.
- **Console after screen mode.** `con_write` moved its column a byte at
  a time, so a plain DISPLAY of national text after a positioned
  statement put what followed too far right. It now moves by display
  width.
- **Refused, as MOVE refuses them:** a national SOURCE, FROM or VALUE
  into a field that is not national, a national field's input into an
  item that is not national, and USAGE NATIONAL on PICTURE X.

bad/std2002-nat-rw, -nat-screen and -nat-accept-at are gone (now
valid). -nat-rw-source and -nat-screen-from keep their refusals under
the MOVE rule. New: -nat-rw-value, -nat-rw-usage-x and -nat-screen-to.
2002/natreport reads its print file back. 2002/natscreen types UTF-8:
a combining acute, a wide character refused at the seventh column,
positioned DISPLAY and ACCEPT. Harness 283/283; -std=85 byte-identical
on all 229 Open Systems programs; majesty PASS; majesty-functions PASS;
Open Systems paper unchanged.

### 93. Bit arrays, part two: INDEXED BY, element reference modification, SYNCHRONIZED, bit-group VALUE, B-NOT ALL (2026-09-28)

Five of the USAGE BIT refusals from ISSUES-78 and -84 lifted:

- **INDEXED BY on a bit array.** An index-name holds an occurrence
  number, as a data-name subscript does. So SET, SEARCH and index-name
  subscripts (`pr(px)`, `pr(px - 1)`) take the path the other
  subscripts already took. The refusal guarded nothing.
- **Reference modification of an element**, `pr(i)(k:1)`. Positions
  count bits within the element (8.4.3.3.4 rule 5a). The compile-time
  bounds were already checked against the element's bits. The start in
  the array is `(i - 1) * bits + start`; with both literal it is folded,
  otherwise `emit_bitelem_start` works it out on the numeric stack. That
  leaves r11, the subscript accumulator, alone. A part with no length
  runs to the element's end, not the array's. EC-BOUND-REF-MOD checks
  the start within the element. The old runtime-subscript arithmetic
  (`emit_sub_index`) is gone into the same helper.
- **SYNCHRONIZED** on a bit item or bit group: implementor-defined
  (8.5.1.6.3). It starts at a byte, and what follows it at the next byte.
- **VALUE on a bit group** (GROUP-USAGE BIT): a boolean literal, ZERO or
  ALL, laid over the group's bits from the first, aligned left and
  zero-filled. The subordinate items take their defaults first.
- **B-NOT of an ALL literal**: `cob_bnot` already inverted an ALL entry
  in place, keeping it ALL, so only the refusal went. It stays an ALL
  literal for 8.8.2 rule 4 and 14.9.8.3 rule 3. `COMPUTE w = B-NOT ALL
  B"011"` is refused as an ALL literal alone, as the rule says.

Found on the way: a group of bit items without GROUP-USAGE BIT is an
alphanumeric group (13.18.29.4 rule 3), and a boolean VALUE on it
stored the literal's characters ('1' for B"1100101"). It is now refused
by name.

2002/bitarray2; bad/std2002-bitelem-refmod-past and
-bool-value-alnum-group new; -bit-refmod and -usage-bit gone (now
valid). Still refused: OCCURS DEPENDING ON on a bit array, OCCURS on a
bit group, a character item redefining a mid-byte bit item. Harness
284/284; -std=85 byte-identical on all 229 Open Systems programs;
majesty PASS; majesty-functions PASS; Open Systems paper unchanged.

### 94. The Stage B code review (2026-09-28) -- findings, to be worked off

After ISSUES-62 to -93 went in over two days, three independent
reviewers read the code against the 2023 text, one area each. Every
"bug" and "standard" item was reproduced with a program; they are kept
under `cobol/out/review/{national,boolean,except}/` (gitignored) and
become regression tests as they are fixed. Checked off here as they go.

**NATIONAL, term, reports, screens**
- N1 bug: a binary or packed numeric sender to or compared with a
  national item loses digits (as_national's max is bytes, not digits).
- N2 bug: screen ACCEPT rewrites a national item whose cluster has more
  than 4 code points (nat_clusters drops the rest; the commit writes it
  back). Data loss on Enter alone.
- N3 bug: the term shadow swallows the character after a full cluster
  (term_cell_join failing still returns). Both copies.
- N4 bug: the term service's UTF-8 decoders do not validate (overlong,
  surrogates, > U+10FFFF; READ_CHAR eats the byte that broke a
  sequence). Both copies.
- N5 bug: UNSTRING without DELIMITED BY counts `room` in bytes for a
  national source into a SIGN SEPARATE numeric national receiver.
- N6 bug: class conditions (NUMERIC, ALPHABETIC*, class-names) test
  bytes on PIC N items.
- N7 bug: three cluster/width models disagree (con_write,
  nat_clusters, the shadow); all join anything after ZWJ, where UAX #29
  GB11 joins only Extended_Pictographic.
- N8 standard: a national LINE SEQUENTIAL READ of a long line gives 04
  and drops the rest; the comment cites 2023 14.9.30 rule 15, which
  says 06 and keeps the rest for the next READ.
- Suspicions: record-oriented RW print file cuts a national line at
  bytes; restore skips a space cell carrying marks; \n does not reset
  the shadow's join state; fn_var_result caps at 256 national chars; a
  screen field whose text is wider than the field cannot be edited.
- Refactor: one shared Unicode header (validating decoder, encoder,
  surrogate read/write, width, UAX #29 cluster step) for libcob, the
  compiler, the emulator and QEMU -- generated from libutf's DFAs
  (width, GCB, Extended_Pictographic), replacing gen_term_width.py's
  table; sym_nat_usage/sym_chars/desc_nat_chars predicates;
  sfield_cols/rfield_cols -> pic_cols; nat_fig_lit/all_lit_national;
  one nat_to_utf8_alloc; the QEMU shadow copy kept in sync by a check.

**Exceptions, the exception-checking PERFORM, EXIT, function refmod**
- E1 bug: a function's computed-position refmod evaluates the start and
  length after the function, clobbering fnbuf and fn_var_len.
- E2 bug: a computed refmod length of 0 on a function result returns
  the whole result, no EC-BOUND-REF-MOD (0 is the "no length" sentinel).
- E3 bug: (start:) on a run-time-length result is never checked.
- E4 bug: WHEN EXCEPTION EC-USER does not turn on EC-USER names met later.
- E5 bug: WHEN EXCEPTION EC-ALL leaves g_ecuser_on set past END-PERFORM.
- E6 bug: PERFORM WITH LOCATION outlives END-PERFORM for names already on.
- E7 bug: the implicit TURN does not override an earlier per-file OFF.
- E8 bug: a file's USE AFTER ERROR runs instead of the WHEN phrase
  (14.9.28 rule 17: matching USE declaratives are ignored).
- E9 bug: .Lecpr/.Lecpf are static; a recursive activation in a WHEN
  phrase overwrites its caller's resume point.
- E10 bug: sentence error recovery does not restore the ECP / PERFORM
  / checking state (spurious later diagnostics).
- E11 standard: WHEN matching order is not USE rule 3c-3g (14.9.49.4).
- E12 standard: fatal conditions go to WHEN OTHER (14.6.13.1.3 rule 4).
- E13 standard: >>TURN inside an exception-checking PERFORM should be
  refused (7.3.25 syntax rule 5).
- E14 standard: RAISE selecting the active USE declarative should be
  EC-FLOW-USE (14.9.49.4 rule 2).
- E15 standard: PERFORM WITH TEST ... UNTIL EXIT accepted (14.9.28.3 rule 8).
- E16 standard: duplicate exception-name across WHEN phrases (rule 15).
- E17 standard: a non-integer computed refmod position is truncated,
  no EC-BOUND-REF-MOD (8.4.3.3.4 rule 5).
- E18 debatable: EXIT PERFORM / GO TO in a WHEN phrase for a fatal
  condition escapes the termination; decide and document.
- Refactor: one checking-state struct saved and restored whole (the
  partial restores caused E5/E6, and it is where error recovery and
  >>PUSH/>>POP belong); ec_covers(i, c); the implicit TURN built on
  apply_turn's core; one WHEN/USE ranking function; one ECP pre-scan;
  close_para/close_sec helpers; an EC-BOUND-REF-MOD emit helper; grow
  the fixed ceilings (g_ecp[8], 16x16 WHEN names, g_pstk[64], g_ecf[256]).

**BOOLEAN, USAGE BIT, TYPEDEF**
- B1 bug: INITIALIZE of bit items copies whole bytes (clobbers bits of
  neighbours); a bit REDEFINES masks its whole byte.
- B2 bug: INITIALIZE REPLACING BOOLEAN over a bit array writes every
  occurrence to element 1 (init_replace_walk's Ref skips the bit rewrite).
- B3 bug: INITIALIZE REPLACING continues only for the five 85
  categories (BOOLEAN, NATIONAL after the first phrase fail); INITIALIZE
  of a bit-array element is refused as reference-modified.
- B4 bug: an ALL boolean literal beside a run-time-length operand is
  expanded to length 1 (MOVE, comparison, function results).
- B5 bug: a group with a group-level USAGE BIT clause (not GROUP-USAGE
  BIT) is taken as a zero-length boolean (use sym_bitlike everywhere).
- B6 bug: CALL BY REFERENCE of a bit item not starting a byte is
  accepted and passes the wrong bits (14.9.4.3 rule 6).
- B7 bug: shift counts near or past 2^31 crash or are truncated.
- B8 bug: USAGE BIT items over 256 bits compile, then die at run time.
- B9 bug: TYPE: the entry's VALUE loses when written before TYPE
  (13.18.57.4 rule 3).
- B10 bug: a group TYPE is not aligned as a level 1 item (rule 2d).
- B11 standard: boolean relations accept > < >= <= (8.8.4.2.2 format 2).
- B12 standard: THRU on a boolean condition-name (13.18.63.3 rule 29).
- B13 standard: strong-group MOVE too strict as a sender (Table 16),
  too loose elsewhere: VALUE on a strong group, an 88 on it, ACCEPT
  into it, UNSTRING into it.
- B14 standard: non-elementary MOVEs with bit groups convert; 14.9.25.4
  rule 4 says bytes are copied without conversion.
- B15 standard: MOVE ALL "1" to a boolean is refused (rule 7 bars only
  non-boolean characters).
- B16 standard (lower confidence): a shift after B-NOT takes the wrong
  precedence (8.8.2 rule 7b).
- B17 standard (lower confidence): a bit item after a character
  REDEFINES stays in the same bit run.
- Minor: TYPE syntax rules 2 and 5 unchecked; VALUE on a bit group and
  on a subordinate both accepted; negative shift counts silently 0;
  ALIGNED clause not implemented and not listed; a refmod start is
  evaluated twice per operand (twice for side-effecting functions).
- Refactor: bits_get/bits_put (five hand-written packing loops);
  sym_is_bititem and sym_bitlike used everywhere; the bit-array element
  as its own Ref field (ref_resolve_bits, a user_rm flag) instead of
  hidden in the refmod fields; compute a refmod start once into a
  frame slot; one ALL path (the run-time one); boolean stack limits
  agree (32 vs 64); diagnose an unknown TYPE name during expansion.

Work order: the shared Unicode header and the NATIONAL items; then the
checking-state struct and the E items; then the bit Ref refactor and
the B items; then the misstated and missing refusals of
docs/refusals.md.

**Progress, part 1 (2026-09-28): the shared Unicode model; N1-N7.**
`common/s32utf.h` is now the one model for text: a streaming UTF-8
decoder that validates (overlong forms, surrogates, past U+10FFFF) and
gives one U+FFFD per maximal subpart (Unicode 16.0 3.9), re-feeding the
byte that cut a sequence short; UTF-8 and UTF-16BE encoders; libutf's
width, Grapheme_Cluster_Break and Extended_Pictographic DFAs
(`s32utf_tables.h`, generated from ~/utf by `gen_s32utf.py`); a UAX #29
cluster stepper; and tinymux's cluster-width policy (widest code point;
a flag, or anything with U+FE0F, two). `common/test_s32utf.c` runs
Unicode's GraphemeBreakTest over it: 1,086 of 1,093 lines agree, the
other 7 being GB9c, which libutf does not implement either. The test
found a libutf bug on its first run: GB12/13 counts regional
indicators across the whole cluster, so RI Extend RI is one cluster
(`utf_grapheme_next` gives 10 bytes where UAX #29 gives 6); the port
pairs only adjacent ones. Fixed upstream in libutf e46427a, with a
second defect the same GraphemeBreakTest run found there (GB4 was not
applied after a CR, so CR + a mark was one cluster; 30 of its 32
disagreements); libutf now agrees on every line but GB9c.
`term_width.h` and `gen_term_width.py` are gone.

Everything that measured text now uses it: the term service's shadow
(both copies), libcob's nat_clusters, con_write and coding helpers,
and the compiler's nat_lit_cols and UTF converters.

- N1 fixed: nat_cap() sizes a numeric sender by its digits (40).
- N2 fixed: a cluster is a span of the item's code units; the screen
  edit splices units and re-splits, so nothing is capped or dropped. A
  field already wider than its columns stays editable (an edit may not
  make it wider still).
- N3 fixed: clusters come from UAX #29, not "width 0 or after ZWJ"; a
  cell keeps 8 code points. Blanking half a wide cell now clears its
  marks too -- a bug the new regression test found on its first run
  (the repaint re-emitted " ZWJ woman ZWJ girl" after the space).
- N4 fixed: the shadow and READ_CHAR decode through s32utf.h; READ_CHAR
  keeps the byte that cut a sequence short for the next read
  (key_pushback, seen by READ_KEY and KEY_AVAIL too).
- N5 fixed: UNSTRING's room counts the receiver's character positions.
- N6 fixed: class conditions on PIC N test characters; one past U+00FF
  is no digit, letter or class member.
- N7 fixed: one model; GB11 joins only emoji after a ZWJ.
- Suspicions: restore now repaints a space cell that carries marks; a
  line end, carriage return or tab ends the cluster; the field too wide
  to edit is fixed with N2. Still open: a record-oriented RW print file
  cuts a national line at bytes; fn_var_result caps at 256 characters.
- N8 fixed (the user approved the split): under -std=2002 the compiler
  sets 4 in a line sequential file's varying, and READ follows 2023
  14.9.30 rule 15 -- a line longer than the record fills it, status 06,
  the rest left in the read buffer for the next READ (a line exactly
  the record's length, CR LF included, is 00). National records decode
  a character at a time and stop before one that does not fit (a pair
  needs two positions). -std=85 keeps GnuCOBOL's 04 with the rest
  dropped, which majesty reads. 2002/lsrule15; 2002/natfiles' long line
  now reads 06 then its rest.
- Behavior change: utf8_to_nat (NATIONAL-OF, MOVE of alphanumeric to
  national, line sequential READ) now gives one U+FFFD per maximal
  subpart, not per byte. No test's output changed.

Tests: regression/tests/feature-term-clusters (all engines agree),
cobol/tests/2002/natreview and natscreen2. Regression 94/94; kit
differential 80/80; cross-engine 90/94 (the four bug-dbt-intrinsic-bounds
divergences are QEMU's missing fault line, pre-existing); COBOL harness
286/286; -std=85 byte-identical on all 229 Open Systems programs; majesty
PASS; majesty-functions PASS; Open Systems paper unchanged.

**Progress, part 2 (2026-09-28): exceptions -- E1-E18.**
The checking state is one struct, `EcState` (each level-3 condition's
checking and LOCATION, the per-file overrides in a growable list, and
what EC-USER-names not yet met will take), copied and restored whole.
`ec_covers(i, c)` is the one "name covers condition" test and
`ec_turn_c` the one core of a TURN; >>TURN and the implicit TURN both
use them (the review's R2/R3, which would have prevented E4-E7). The two
token pre-scans are one, `ecp_scan` (R1); the WHEN lists, the PERFORM
nesting and the per-file settings grow on demand (R9); the WHEN/USE
match order is one function, `ec_match_rank` (R4); the function
reference-modification check is `emit_fn_rm_check` (R8); the misplaced
comments are back on their functions (R5).

- E1 fixed: a function's computed start and length are evaluated before
  the function.
- E2 fixed: "no length" is -1, so a computed 0 is out of range.
- E3 fixed: (start:) on a run-time-length result notes a start past its
  end, and is checked.
- E4, E5, E6, E7 fixed: the implicit TURN (14.9.28 rule 14) turns on
  only what is not enabled -- for all files, or for the WHEN's file;
  LOCATION goes with it and nowhere else; EC-USER and EC-ALL decide the
  EC-USER-names met later too. After END-PERFORM the state before the
  PERFORM comes back whole (rule 22 -- there can be no TURN inside, E13),
  EC-USER-names first met inside taking the restored later-name setting.
- E8 fixed: inside imperative-statement-1, an EC-I-O condition a WHEN
  takes is tested before the file's USE AFTER ERROR procedures, which
  are ignored for it (rule 17).
- E9 fixed: the resume points are a stack in libcob (cob_ecp_push, _pop,
  _drop), keyed by the PERFORM's id and the activation's frame, so a
  recursive activation's raise does not overwrite its caller's.
- E10 fixed: a failed sentence restores the checking state, the
  exception-checking PERFORM nesting and the PERFORM stack.
- E11 fixed: WHEN phrases match in USE rule 3c-3g's order.
- E12 fixed: a fatal condition never goes to WHEN OTHER (14.6.13.1.3
  rule 4); without a WHEN naming it, the USE declarative, then the end.
- E13 fixed: a TURN inside an exception-checking PERFORM is refused
  (7.3.25.3 rule 5), which settles rule 22's contradiction.
- E14 fixed: RAISE selecting an active USE declarative is EC-FLOW-USE,
  fatal, checked or not (cob_use_push); performing it again lost its
  return.
- E15 fixed: UNTIL EXIT with a TEST phrase is refused (14.9.28.3 rule 8).
- E16 fixed: an exception-name twice in the WHEN phrases is refused
  (rule 15).
- E17 fixed: under EC-BOUND-REF-MOD, reference-modification positions
  are popped by cob_pop_pos, which notes a fraction for the bound check.
- E18 decided: a WHEN phrase for a fatal condition that leaves by EXIT
  PERFORM still ends the run -- the raise is dropped at the PERFORM's
  end, and a fatal one aborts there (14.6.13.1.3 rule 4). A GO TO out of
  the PERFORM altogether escapes it; NOTE 9 of 14.9.28 warns against
  that.

Tests: 2002/ecpreview (E4-E8, E11), ecprecur (E9), ecpfatal (E12),
ecpfatal2 (E18), ecflowuse (E14), fnrmzero (E1, E2), fnrmpast (E3),
rmnonint (E17); bad/std2002-ecp-turn (E13), -until-exit-test (E15),
-ecp-dup (E16), -ecp-recovery (E10). Harness 299/299; -std=85
byte-identical on all 229 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

**Progress, part 3 (2026-09-28): BOOLEAN, bits, TYPEDEF -- B1-B17.**
The bit-array element is resolved by one function, `ref_resolve_bits`,
which every Ref builder calls (parse_ref, INITIALIZE's walk), and a Ref
records `user_rm`, whether the program wrote a reference modification.

- B1 fixed: INITIALIZE sets bit items by MOVE (a boolean zero each), not
  by the template's bytes; the template covers only the bytes of the
  items INITIALIZE sets (`init_cover`, the inverse of the old mask). That
  also fixed a Stage A bug: the old mask left alone the bytes of every
  REDEFINES item, which are the redefined item's too, so INITIALIZE of a
  group never initialized an item another redefined (GnuCOBOL agrees
  with the fix; X3.23 6.16 excludes only the REDEFINES items).
- B2, B3 fixed: REPLACING reaches every element of a bit array; BOOLEAN
  and NATIONAL phrases may follow the first; a bit array's element may be
  INITIALIZEd; and INITIALIZE of a reference-modified item -- the Stage A
  gap of docs/refusals.md -- sets the part as an elementary alphanumeric
  (national, boolean) item.
- B4 fixed: an ALL literal beside a computed-length reference
  modification or a run-time-length function result is repeated at run
  time: comparisons go by the boolean stack, MOVE by cob_bstore, which
  expands ALL to the receiver's positions.
- B5 fixed: sym_bitlike at every site; a group with a group-level USAGE
  BIT clause is alphanumeric.
- B6 fixed: a bit item passed BY REFERENCE must start a byte, with
  literal subscripts and leftmost position (14.9.4.3 rule 6).
- B7 fixed: a shift's count is popped whole by the runtime
  (cob_bshift_pop): L/R by the length or more gives zeros, circular goes
  mod the length.
- B8 fixed: nat_narrow uses a ring of heap buffers past 256 positions.
- B9, B10 fixed: a TYPE's clauses go right after the level and name, the
  entry's own after them, so the entry's VALUE is the one used (rule 3);
  a group type starts a byte, as a level 1 item (rule 2d).
- B11 fixed: boolean relations are EQUAL and NOT EQUAL only, and a
  strongly-typed group holding a boolean item likewise. Two of this
  project's own earlier tests (2002/boolean, boolexpr) used `>` on
  booleans; they were wrong and now say NOT =.
- B12 fixed: THROUGH on a boolean condition-name is refused (13.18.63.3
  rule 29).
- B13 fixed: a strongly-typed group as a MOVE sender goes anywhere a
  group does (14.9.25.3 rule 2 constrains only the receiver); VALUE on
  one (13.18.63.3 rule 1), ACCEPT into one (14.9.1.3 rule 1) and
  UNSTRING into one (rule 4) are refused. The 88 on a strong group the
  review listed is left: no rule found that forbids it.
- B14 fixed: a MOVE between an ordinary group and a bit group copies
  bytes (14.9.25.4 rule 4).
- B15 fixed: MOVE ALL "1" (any ALL literal of 0s and 1s) to a boolean.
- B16 fixed: a shift after B-NOT takes B-NOT's precedence (8.8.2 7b).
- B17 fixed: a character REDEFINES ends a run of bits (8.5.1.6.3).
- Also: the runtime boolean stack grows (it held 32 where the compiler
  allowed 64); an unknown TYPE name is diagnosed as one (a type declared
  later, or itself, 13.18.58.3 rule 2).
- Left open from the review's minor list: TYPE syntax rules 2 and 5;
  VALUE on a bit group together with a subordinate VALUE; a negative
  shift count shifts nothing; the ALIGNED clause (not implemented, now
  listed in docs/boolean.md); a computed reference-modification start is
  evaluated twice per operand (a side-effecting function in it runs
  twice).
- The shared Unicode header also took libutf 92968c8 (from TinyMUX):
  GB11 allows exactly one ZWJ between pictographs; the stepper keeps a
  three-state GB11 tracker, and test_s32utf.c has the case, which
  GraphemeBreakTest lacks.

Tests: 2002/boolreview, strongsend; bad/std2002-bool-relation,
-bool-88-thru, -strong-value, -strong-accept, -strong-unstring,
-bit-byref, -type-later. Harness 308/308; regression 94/94; kit
differential 80/80; cross-engine 90/94 (the four pre-existing QEMU
fault-line divergences); -std=85 byte-identical on all 229 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

### 95. The refusal survey's fixes: misstated and missing refusals, the Stage A gaps (2026-09-28)

docs/refusals.md sorted the compiler's refusals. Its class 1 and the
COBOL 85 part of class 2, done:

- **Forbidden, and now says so.** A reference-modified STRING receiver
  cites X3.23-1985 STRING rule 3 and 2023 14.9.43.3 rule 4 instead of
  "not implemented". Items after an OCCURS DEPENDING ON table in its
  record are refused at the declaration (85 OCCURS format 2 rule 10;
  2023 13.18.38.3 rule 22) -- before, only a MOVE or operand of the group
  complained. None of the 229 Open Systems programs, majesty or CCVS-85
  declares one.
- **Forbidden, and was accepted.** Under -std=85 a reference-modified
  UNSTRING sending item is refused (85 UNSTRING rule 7; 2023 dropped it).
- **Stage A gaps closed.** CALL BY CONTENT of a reference-modified item
  (its length from cob_refmod_len; a bit part still refused). A REPORT
  SECTION in a contained program: each unit's reports start at
  g_report_base, where the contained program's used to overwrite its
  container's. The Report Writer CODE clause (85: a two-character
  literal; 2023: a literal or an identifier, taken at each body group),
  kept by the runtime per report (cob_rw_code) so no report block
  changes shape; with it, FD REPORTS ARE of several reports, INITIATE and
  TERMINATE of several report-names, and the rule that CODE is on each
  report of a file or none -- all three missing before.
- **Dead code.** SUPPRESS's entry in the verbs refused as "not
  implemented" (SUPPRESS is implemented).
- **GnuCOBOL.** 4.0-early-dev writes an empty print file for a report
  with CODE; fixed/rwcode is ours alone, the divergence in oracles.md.

Tests: fixed/bycontentrm and rwnested (the oracle agrees), fixed/rwcode
(no oracle); bad/odo-followed, string-refmod-receiver,
unstring-refmod-85, rw-code-partial. CCVS-85 unchanged: 348 programs,
8068 of 8175 tests, all 348 matching GnuCOBOL. Harness 315/315; -std=85
byte-identical on all 229 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

### 96. The rule-by-rule sweep, one section at a time (2026-09-28)

docs/conformance/ holds one page per section of the 2023 text swept:
every syntax and general rule, paraphrased, with a disposition -- a test,
a refusal test, not applicable by ruling, a named gap, or an
implementor's ruling. The CCVS-85 suite tests what a compiler must
accept, almost never what it must refuse, so the sweep is where the
unenforced rules turn up.

**14.9.14 EXIT** (docs/conformance/exit.md). Found and fixed:

- rule 1 (and X3.23-1985 EXIT rules 1-2): a simple EXIT not alone in its
  paragraph was accepted. Now refused; no program in the Open Systems
  suite, CCVS-85 or majesty breaks it.
- rule 2 (85 the same): EXIT PROGRAM in a GLOBAL declarative, accepted;
  refused.
- rule 7: EXIT PROGRAM in a function, accepted; refused.
- X3.23-1985 EXIT PROGRAM rule 1 (not in 2023): EXIT PROGRAM must end its
  run of imperative statements; refused under -std=85.
- general rule 2 (85 general rule 1 the same): EXIT PROGRAM in the run
  unit's first program ended the run; it continues, as CONTINUE. libcob
  counts program activations in cob_perform_enter/leave (cob_called).
  This changes the -std=85 code of the 12 Open Systems programs that
  hold an EXIT PROGRAM -- exactly those -- and not their output: all are
  called subprograms. GnuCOBOL agrees (fixed/exitprog).
- rules 3 and 6, EXIT PROGRAM RAISING: a gap, now refused by name
  instead of "'raising' is not a COBOL verb".
- the EXIT PERFORM CYCLE message read backwards; reworded.

Tests: fixed/exitprog (the oracle agrees); bad/exit-not-alone,
exit-program-global, exit-program-not-last, std2002-exit-program-function,
-exit-paragraph-nopara, -exit-program-raising. CCVS-85 unchanged.
Harness 322/322; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

**14.9.28 PERFORM** (docs/conformance/perform.md). Found and fixed:

- syntax rule 2: PERFORM ... TIMES with a non-integer item, accepted;
  refused (85 rule 4 the same).
- rules 4-6 (85 rules 7-9): the index-name VARYING constraints and a
  zero BY literal, none checked; refused.
- rule 8: UNTIL EXIT as a VARYING phrase's condition said "'exit' is not
  declared"; now cites the rule.
- rule 11 (85 the same): a THRU range across declarative sections,
  accepted; refused.
- X3.23-1985 rule 2 (not in 2023): an in-line PERFORM VARYING with AFTER,
  accepted under -std=85 (GnuCOBOL accepts it too); refused. No program
  in the Open Systems suite, CCVS-85 or majesty uses it; four of this
  project's own tests did (fixed/control, free/vary4, warn/clean-85,
  warn/every-point) and now PERFORM a paragraph instead, their output
  unchanged.
- general rule 3: EC-RANGE-PERFORM-VARYING was never raised; it is, for
  an index-name set FROM an identifier that is not positive.
- general rule 16: nothing kept a GO TO, EXIT PARAGRAPH or EXIT SECTION
  from leaving a FINALLY phrase; refused.

Tests: 2002/perfvary; bad/perform-times-nonint, perform-by-zero,
perform-index-from, perform-from-index, perform-thru-decl,
perform-inline-after-85, std2002-until-exit-varying,
std2002-finally-goto. CCVS-85 unchanged; harness 331/331; -std=85
byte-identical on all 229 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

**14.9.29 RAISE and 7.3.25 TURN** (docs/conformance/raise.md, turn.md).
RAISE: nothing new. TURN: syntax rule 3 (an exception-name and
file-name pair twice in one directive) was accepted; refused. General
rule 5 (a TURN inside a statement governs what follows it in the source,
the ELSE branch included) had no test; 2002/ecturnstmt. Tests:
bad/std2002-turn-dup, std2002-turn-unknown; 2002/ecturnstmt.

**14.9.49 USE** (docs/conformance/use.md). Found and fixed: syntax rule
1 (USE right after its section header, a sentence by itself -- neither
half checked), 2 (no USE on a sort or merge file), 3 (a declarative
refers to no nondeclarative procedure) and 4 (a declarative procedure
named from outside its section only by PERFORM) -- the prescan now marks
each paragraph in_decl -- and 10 (no GENERATE, INITIATE or TERMINATE in
a USE BEFORE REPORTING procedure). Two format 3 USE statements for one
exception-name were refused, which no rule supports; general rule 3
takes the first. The open-mode and file duplicate messages cite rules
7-8. Gaps named: format 3's FILE phrase (rules 13-14, general rule 3c-d)
and the run-time EC-FLOW-REPORT (general rule 10).

Tests: 2002/usedupec; bad/use-not-first, use-not-alone, use-sort-file,
use-refers-main, use-goto-into, rw-use-generate. CCVS-85 unchanged;
harness 341/341; -std=85 byte-identical on all 229 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

**14.9.25 MOVE** (docs/conformance/move.md). Table 16 checked cell by
cell, 90 programs; eleven forbidden moves were accepted. Found and
fixed:

- syntax rule 10 / Table 16 (85 general rule 3a-b): alphabetic and
  alphanumeric-edited to numeric or numeric-edited, national-edited to
  numeric, integer, noninteger and numeric-edited to alphabetic, all
  accepted; refused. A noninteger item to an alphanumeric one stays
  accepted under -std=85 (NIST NC105A/NC114M/NC124A, the ruling of
  2026-08-31) and is refused under -std=2002. move_invalid() holds the
  table and syntax rules 1, 5, 6 and 8.
- rule 1 (85 rule 4): an index or pointer item as a MOVE operand,
  accepted; refused. Rule 5: HIGH-VALUE and the other alphanumeric
  figuratives to a numeric item under -std=2002; rule 6: ZERO to an
  alphabetic item; rule 8: a binary-char/-short/-long sender to a
  non-numeric receiver -- all accepted, all refused.
- 14.7.6 rules 2 and 4 (85 6.4.3 the same): MOVE CORRESPONDING moved
  pairs whose MOVE is invalid, and index items; such pairs do not
  correspond and are skipped now. GnuCOBOL refuses the statement.
- general rule 1 (85 general rule 2): the sender was identified again
  for each receiver -- `MOVE te (b) TO b, ce (b)` stored te (1) in
  ce (1), not te (2), and `MOVE FUNCTION RANDOM TO r1 r2` drew two
  numbers. parse_move now collects the receivers first; when one ahead
  of the last shares storage with a subscript or a reference modifier's
  start, the sender is copied to a compiler-made record (ftemp_new);
  for an OCCURS DEPENDING ON group the DEPENDING ON item is copied
  instead; a function is evaluated once into a static copy
  (Opnd.fsaved). A computed reference-modification length over such an
  item is refused as not implemented. GnuCOBOL gets the ODO case wrong
  (docs/oracles.md).
- zero-length literals ("", X"", N"", B"") were accepted in both
  editions; they are COBOL 2014's (85: 1 through 160 characters; 2002
  8.3.1.2.1.2, .3.2, .4.2 rule 1). Refused.
- -std=2002 refused a 19-digit item with the 1985 limit's message; 2002
  allows 31. Named as a gap (the arithmetic is 64-bit).

Found on the way, not a MOVE rule: **a use-after-free in the compiler.**
sym_new grew g_sym with realloc, but Sym pointers live the whole
compile (every Ref a statement holds while parsing, odo_dep_sym, a
file's keys, g_returning, UCall), and ftemp_new makes records in the
PROCEDURE DIVISION. A COMPUTE whose user-function call grew the table
read freed memory (AddressSanitizer, the caller padded to 115 items so
the table crossed a doubling). The table is now reserved once with mmap
(1M entries of address space, committed as it grows) and never moves.
An ASan build over every test program, and the padded reproducer at
90-600 items, is clean.

Tests: free/moveonce (the oracle agrees with its documented
divergence), free/moverules (the oracle agrees), 2002/movecorr (no
oracle); bad/move-alpha-to-num, move-edited-to-alpha, move-space-to-num,
move-zero-to-alpha, move-index, move-refmod-len-recv, empty-literal,
std2002-move-nonint-alnum, -move-natedited-num, -move-highvalue-num,
-move-all-edited, -move-binchar-alnum, -move-pointer, -empty-boolean.
CCVS-85 unchanged; -std=85 byte-identical on all 229 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

**National and boolean data: 13.18.29 GROUP-USAGE, 13.18.60 USAGE (BIT,
NATIONAL), 13.18.40 PICTURE (1, N), 8.3.3.4-5 literals**
(docs/conformance/national-boolean.md). Found and fixed:

- GROUP-USAGE rule 1: a strongly-typed group with GROUP-USAGE,
  accepted; refused. Rules 2-3: a GROUP-USAGE NATIONAL group inside a
  bit group (or the reverse), and a subordinate group with a USAGE of
  its own, were refused under the wrong rule, naming an item below;
  now named at the group.
- USAGE rule 20: PIC N with USAGE DISPLAY -- its own or its group's, in
  the data division, a report group or a screen entry -- accepted;
  refused. Rule 17: USAGE in a screen entry was not parsed; DISPLAY and
  NATIONAL are taken (USAGE NATIONAL on a non-N screen item is a named
  gap). Rule 7's message was "expected 'display'". The USAGE rules
  were cited as 13.18.66 (2002's numbering); now 13.18.60, in the tests
  too.
- PICTURE: a picture mixing 1 or N with other symbols, and a zero
  repeat count, were refused as "not valid at character 1"; the
  message now names 13.18.40.4 rules 8-10 or 13.18.40.3 rule 6 (85
  VI-30 general rule 7).
- literals: 2002 (and 85) cap alphanumeric, boolean and national
  literals at 160 positions; any length was accepted. Refused. X"..."
  under -std=85 stays, an extension majesty uses (a ruling on the page).

Tests: 2002/natlitquote (no oracle: GnuCOBOL's national support is
unfinished); bad/std2002-groupusage-strong, -groupusage-mixed,
-groupusage-subusage, -picn-display, -picn-group-display, -pic-bool-mixed,
-pic-nat-mixed, -screen-usage, -rw-usage, -national-literal-161,
pic-zero-count, literal-161. CCVS-85 unchanged; harness 371/371; -std=85
byte-identical on all 229 Open Systems programs; majesty PASS;
majesty-functions PASS; Open Systems paper unchanged.

**13.18.40 PICTURE and 13.18.8 BLANK WHEN ZERO** (docs/conformance/picture.md).
The analyser checked the symbols' meaning but almost none of the order
and combination rules: 99CRCR, 9V9V9, S9S9, Z*9, +99CR, 9+9, 9$9,
++$$9, ZZ.Z9 and .$$ all compiled, a picture could be any length, and
BLANK WHEN ZERO took alphanumeric, COMP, S and * pictures. Now:
pic_rules() checks 13.18.40.3 rules 12, 16-21, 23, 25-27 and 29 with
messages that cite them; pic_precedence() checks every ordered pair of
symbols against Table 10, read off the 2023 PDF by mark position
(pdftotext -bbox; the two pages differ by 17 points), and rule 12a; the
length is 30 characters under -std=85 and 50 under -std=2002;
bwz_check() takes the BLANK WHEN ZERO rules for data, report and screen
items.

Checked by differential: 7,368 generated pictures against GnuCOBOL 4
(-std=cobol85, which implements the chart). 7,313 agree; the 55 that
do not were each settled by the text, which sides with this compiler
in every case (docs/oracles.md). tests/pic-differential.sh reruns it
and fails if the count moves; 42 of the cases join tests/pictures.txt.

Tests: free/picedit (editing at its edges; the oracle agrees);
bad/pic-crdb-twice, -s-not-first, -p-and-point, -z-and-star,
-sign-exclusive, -fixed-sign-middle, -currency-middle, -two-floating,
-9-before-z, -z-past-point, -float-after-point, -no-digit-symbol,
-precedence, -too-long, bwz-star, -sign, -alnum, -comp. CCVS-85
unchanged; harness 390/390; -std=85 byte-identical on all 229 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

**13.18.60 USAGE, the rest** (docs/conformance/usage.md). Found and
fixed: rule 10 (85 USAGE rule 5) -- an index data item in DISPLAY, ADD,
COMPUTE and the other statements outside SEARCH, SET, conditions,
function arguments and USING was accepted; index_ref_check() in
parse_ref refuses it, with g_fn_depth for function arguments and
g_in_proc saved around contained programs. Rules 8-9 likewise for
pointers. Rule 11 (85 rule 7): a level 88 under an index or pointer
item; rule 14: a pointer below level 1; 85 rule 6 (2023 13.16.3 rule
10, 13.18.32.3 rule 3): VALUE, JUSTIFIED, BLANK WHEN ZERO on an index
or pointer item, SYNCHRONIZED on an index under -std=85 -- all
accepted, all refused. BINARY-SHORT and BINARY-LONG took no SIGNED or
UNSIGNED phrase; they do. SET pointer TO NULL did not compile (emit_move's
numeric path took only ZERO) and no pointer compared equal to NULL
(opnd_args expanded NULL as alphanumeric bytes); both fixed. Named
gaps: BINARY-DOUBLE (19 digits), ADDRESS OF (misread as a qualified
name before), ALLOCATE/FREE. Rules 1 and 3 have clearer messages.

Tests: 2002/binranges, 2002/pointerset (the oracle agrees with both);
bad/usage-88, index-ref-display, index-ref-arith, index-88,
index-value, index-sync-85, std2002-pointer-ref-display, -pointer-88,
-pointer-level, -pointer-value, -binary-double, -address-of. CCVS-85
unchanged; harness 404/404; -std=85 byte-identical on all 229 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

**ADDRESS OF and BASED** (2002 8.4.2.11, 13.16.5; 2023 8.4.3.11, 14.9.39
formats 7 and 10), the gap the USAGE sweep named. ADDRESS OF identifier
is an operand kind of its own (O_ADDR), taken by SET, CALL BY VALUE and
relations; emit_ptr_value() gives its address, a pointer's content or
NULL. A BASED record (level 01/77, WORKING-STORAGE or LINKAGE) is an
indirect record like a LINKAGE one: a cell, NULL at first, that SET
ADDRESS OF fills; ADDRESS OF such a record is the cell's content, NULL
included; with EC-DATA-PTR-NULL checked, a reference through a NULL cell
raises it. SET ADDRESS OF a non-based LINKAGE record is accepted (IBM,
GnuCOBOL; a ruling on the usage page). SET pointer UP/DOWN BY n moves it
in bytes. Pointer relations are EQUAL/NOT EQUAL between data pointers
only (8.8.4.2.3 rule 5), which also turns away `p = 5`. Gaps named:
ADDRESS OF BY REFERENCE/CONTENT, BASED in LOCAL-STORAGE, ALLOCATE/FREE,
EC-BOUND-PTR.

Tests: 2002/addressof (the oracle agrees), 2002/ecptrnull;
bad/std2002-address-of-display, -set-address-ws, -pointer-lt,
-address-of-byref, -pointer-cmp-num, address-of-85. CCVS-85 unchanged;
harness 411/411; -std=85 byte-identical on all 229 Open Systems
programs; majesty PASS; majesty-functions PASS; Open Systems paper
unchanged.

**ALLOCATE, FREE; ADDRESS OF BY REFERENCE/CONTENT** (2002 14.8.3,
14.8.14). libcob cob_allocate/cob_free keep a list of the blocks the run
unit holds (calloc: zeroed, pointers NULL); FREE sets the pointer NULL,
leaves NULL alone, and reports anything else for EC-STORAGE-NOT-ALLOC;
ALLOCATE raises EC-STORAGE-NOT-AVAIL when none is to be had but not for
a count of 0 or less (GR 2; the first cut raised it). cob_pop_alloc_size
rounds a fractional count up (GR 1). ALLOCATE and FREE joined is_verb,
without which a FREE after a MOVE was read as another receiver. ADDRESS
OF BY REFERENCE and BY CONTENT pass a compiler-made pointer record (the
unique data item of 8.4.3.11 GR 1). A VALUE clause in a BASED entry,
refused in the previous step, is allowed again: nothing forbids it and
INITIALIZE ... TO VALUE is what reads it. The INITIALIZE WITH FILLER /
DEFAULT refusal said "is COBOL 2002" under -std=2002; it says "not
implemented" now. Gap named: ALLOCATE data-name INITIALIZED.

Tests: 2002/allocfree, 2002/addressofarg (the oracle agrees);
bad/std2002-allocate-not-based, -allocate-no-returning,
-allocate-returning-alnum, -allocate-based-initialized,
-free-not-pointer; bad/std2002-address-of-byref retired. CCVS-85
unchanged; harness 417/417; -std=85 byte-identical on all 229 Open
Systems programs; majesty PASS; majesty-functions PASS; Open Systems
paper unchanged.

### 97. Performance: the comparison kernels, and what they found (2026-09-29)

bench/vs/ holds nine kernels for comparing compilers (arithmetic, MOVE,
editing, STRING/UNSTRING/INSPECT, SEARCH, sequential and indexed files,
SORT, a control-break report): standard COBOL 85, every iteration
dependent on the loop index, a printed checksum. Built with this
compiler and with GnuCOBOL 3.2 and 4.0, all three print the same bytes.
Two kernels ran far behind the rest, and instruction-count ablation
(slow32-fast, one statement added at a time) found an algorithm in each:

- **SEARCH ALL was a linear scan.** By design -- the first entry
  satisfying the WHEN is the one a binary search reports when the keys
  are unique -- but O(n): 66,965 instructions for a search of 2,000
  entries. The OCCURS KEY names were parsed and thrown away. Now the
  table keeps them (Sym okey/okey_desc), and a SEARCH ALL whose one WHEN
  is key (index) = value joined by AND, over the leading declared keys
  of a one-dimension table (2023 14.9.37.3 rules 8 and 11), is a binary
  search steered by each key's < and > in declared order, ASCENDING or
  DESCENDING, bounded by OCCURS or DEPENDING ON: 600 instructions. Other
  shapes keep the scan. The first cut kept the middle in SLOT_A, which
  the key comparisons also use; free/search looped until the middle got
  a slot of its own.
- **INSPECT CONVERTING was O(length x alphabet).** cob_inspect_convert
  registered one single-character replacing phrase per FROM character and
  the general pass tried each at every position: 56,002 instructions for
  60 characters converting a-z. Single-byte data now goes through a
  256-byte table (built from the right, so the first occurrence in FROM
  wins), applied over the BEFORE/AFTER range in one sweep, and kept while
  FROM and TO are byte-for-byte the same: about 2,500. National data keeps
  the phrase pass.

Per iteration, kstring went from 64,599 instructions to 11,675 and
ksearch from 75,990 to 9,624. What remains is constant factors, the
candidates for native DBT hooks or for code that stays out of libcob's
decimal stack: FUNCTION MOD in a COMPUTE (~1,500-1,900), a serial SEARCH
(~170 an element), INSPECT TALLYING (~60 a character), STRING and
UNSTRING (~2,200 each), a binary 9(9) moved to an 8-digit DISPLAY item
(382).

Also: bench/bsort.cbl named a paragraph SUM (reserved); renamed.

Tests: 2002/searchall (the oracle agrees). Harness 418/418, CCVS-85
unchanged, -std=85 byte-identical on all 229 Open Systems programs,
majesty PASS, majesty-functions PASS, Open Systems paper unchanged.

### 98. The CALL parameter family (2026-09-29)

BY VALUE and OPTIONAL parameters, OMITTED arguments, more than eight
arguments, and PROCEDURE DIVISION RETURNING for a program -- the gap
docs/refusals.md ranked first. docs/conformance/call.md has the rules
and the carriage:

- stack arguments: 9-16 at the callee's entry sp + 4k, the C ABI's
  place; the caller stages them in slots and copies them to an outgoing
  area reserved around the jal (the frame's own lr is at sp + 0). The
  callee reads parameters past the eighth above its frame; up to 32.
- BY VALUE parameters: a copy in the activation's frame (g_frame grows
  by their sizes), the value stored as into the item -- so a RECURSIVE
  program's value parameter is its own (the test computes 6! that way).
- OMITTED passes NULL; `cob_call_nargs`, set by every -std=2002 CALL and
  read at entry by a program with OPTIONAL parameters, makes a trailing
  parameter not passed omitted too (14.9.4 GR 11). IS [NOT] OMITTED
  (8.8.4.8); EC-PROGRAM-ARG-OMITTED when checked.
- program RETURNING: the caller's item's address in `cob_call_retaddr`;
  `cob_call_returned`, set by every -std=2002 program at exit, tells a
  COBOL result (in place) from a C one (r1), so the C bridge's CALL
  RETURNING keeps working. None of this is emitted under -std=85: the
  229 Open Systems programs compile byte-identical.

Tests: 2002/callparams (ten arguments, BY VALUE, OMITTED in the middle
and trailing, recursion; the oracle agrees), 2002/callreturning (no
oracle: GnuCOBOL 4 does not implement program RETURNING), 2002/callc (a
C function with ten arguments, its r1 result); bad/std2002-using-twice,
-using-value-alnum, -using-value-optional, -call-omitted-value,
-omitted-not-param, -call-17-args, -returning-using, -returning-ws,
returning-program-85, 2002/ecargomit. The first test used a BY VALUE binary argument
into DISPLAY and COMP-3 parameters; 14.8.2.3.3 rule 1 makes that
non-conforming (the same length), and GnuCOBOL copies bytes where this
compiler converts, so the test was made conforming. Harness 430/430,
CCVS-85 unchanged, -std=85 byte-identical on all 229 Open Systems
programs, majesty PASS, majesty-functions PASS, Open Systems paper
unchanged.

### 99. RETURN-CODE, and -warn-extensions (2026-09-29)

RETURN-CODE, the IBM / Micro Focus / GnuCOBOL special register, taken as
a dialect extension (the user's ruling: in, with a way to warn where
strict compliance is wanted). A PIC S9(9) BINARY item whose storage is
libcob's cob_return_code, shared by the run unit (GnuCOBOL's model and
width). A unit that names it stores what each CALL without RETURNING
returns (a COBOL program's RETURN-CODE, a C function's result) and
returns it from its own exit; cob_stop_run exits with it when STOP RUN
gives no status. A unit that never names it compiles as before, so
-std=85 output is unchanged (229 Open Systems programs byte-identical).

-warn-extensions: class E behavior points now call bp() and warn under
it, the way -warn-74 governs classes M, O and N. BP-E1 is RETURN-CODE;
the other class E rows (docs/behavior-points.md) are still registered
only. Gate 4 runs tests/warn/ext-*.cbl under the new switch.

Also fixed, found writing the tests: a contained program with BY VALUE
parameters (ISSUES-98) left its frame size in g_frame, and the containing
program's exit -- emitted after its contained programs are compiled --
restored that size instead of its own. g_frame, g_prog_ret and
g_uses_rc are now saved around a contained unit.

Tests: free/returncode (the oracle agrees, default dialect),
2002/callc (a C result into RETURN-CODE), 2002/nestedvalue (the
oracle agrees), warn/ext-return-code. Harness 434/434, CCVS-85
unchanged, majesty PASS, majesty-functions PASS, Open Systems paper
unchanged.

**-warn-extensions, the rest of class E** (same day). The registered
extensions are now points BP-E2..E13, each warning for the edition where
it leaves the standard: GOBACK, BINARY-CHAR/-SHORT/-LONG, POINTER,
hexadecimal literals, CALL BY VALUE/RETURNING, the SCREEN SECTION and
free-form source under -std=85 only (standard from 2002); COMP-3, COMP-5,
COMP-1, GnuCOBOL's SIGNED-INT family, STOP RUN identifier/RETURNING,
positioned DISPLAY/ACCEPT, LINE SEQUENTIAL and `_` in user words under
both. On majesty's gl008 the switch reports free form, LINE SEQUENTIAL,
GOBACK and COMP-3/5 -- all true. Tests: warn/ext-every (all thirteen),
warn/ext-clean (a standard 85 program, silent). Harness 436/436; the
gates as above.

### 100. ALPHABET ... IS EBCDIC -- collating sequence and CODE-SET (2026-09-29)

The ruling of 2026-09-28: an alphabet is a collating sequence and a
code set, not the machine's code, so EBCDIC is honoured. It is code page
037, IBM's reference EBCDIC, as the Latin-1 <-> CP037 bijection
(g_cp037, generated from Python's codec). As a collating sequence the
alphabet's rank table is that code, which the existing PROGRAM
COLLATING SEQUENCE and SORT COLLATING SEQUENCE machinery takes as it is.
As an FD's CODE-SET (record sequential files; others refused as not
implemented) the file image carries two 256-byte tables, code_out and
code_in (two new words at the end of cob_file), and libcob converts at
READ (in place), WRITE and REWRITE (a copy, the record area keeping the
machine's code), SORT USING/GIVING included through cob_read/cob_write.
The 85 rules are enforced: no literal alphabet as a CODE-SET (rule 2),
every elementary item USAGE DISPLAY and signed numbers SIGN SEPARATE
(rule 1; 2023 13.18.13.3 rule 3a).

The two image words change the -std=85 output of every program with a
file: of the 229 Open Systems programs, 224 differ by exactly those two
zero words per file image and nothing else, 5 are unchanged.

Tests: free/ebcdic (collation, the raw bytes of a CODE-SET record --
checked against Python's cp037 -- the round trip, a SORT; the oracle
agrees, default dialect, since EBCDIC is an implementor-name in 85);
bad/codeset-literal, -comp, -sign, -lineseq. Harness 441/441, CCVS-85
unchanged, majesty PASS, majesty-functions PASS, Open Systems paper
unchanged.

### 101. The arithmetic statements sweep (2026-09-29)

ADD, SUBTRACT, MULTIPLY, DIVIDE and COMPUTE against 2023 14.7.7 and
14.9.2/.8/.12/.26/.44 and the 85 text. The common rules probed first --
item identification (a receiver identified as it is reached, so ADD 1 TO
i t (i) adds to the new i's element; senders once, at the start), SIZE
ERROR leaving only the receiver that overflows unchanged, the REMAINDER
from the unrounded quotient -- all hold and agree with GnuCOBOL.

Found and fixed so far:
- the composite of operands (85 6.4.4 rule 2 and each statement's rule
  3: ADD/SUBTRACT every operand but the GIVING items, CORRESPONDING by
  pair, MULTIPLY/DIVIDE the receiving items; COMPUTE exempt) was not
  checked. arith_composite() now refuses a composite past 31 digits in
  both editions (past what any edition allows, and past the 64-bit
  arithmetic). 19-31 under -std=85: the only one in the corpora is
  majesty's dist01 (a SUBTRACT, 15 integer digits with 4 decimals, whose
  values fit), so it is taken and a strict build is told: BP-E14 under
  -warn-extensions. Under -std=2002, 19-31 is legal (the 31-digit gap).
  None in the Open Systems suite or CCVS-85.
- DIVIDE ... REMAINDER with more than one GIVING item was accepted;
  refused (85 DIVIDE formats 4-5).
- ROUNDED MODE was called COBOL 2002; it is 2014's (2002 has no MODE).

Tests: bad/arith-composite, bad/divide-remainder-two, bad/rounded-mode
(reworded), warn/ext-every (BP-E14). Harness 443/443, CCVS-85
unchanged, -std=85 byte-identical on all 229 Open Systems programs,
majesty PASS, majesty-functions PASS, Open Systems paper unchanged.
EC-DATA-INCOMPATIBLE (14.6.13.2 rule 2; MOVE GR 6d1), never raised
before: with the condition checked, a numeric DISPLAY, packed or
national sending item whose content fails the NUMERIC class test raises
it -- the operands of ADD, SUBTRACT, MULTIPLY and DIVIDE (an ADD TO /
SUBTRACT FROM receiver too, which is summed), COMPUTE's operands as the
expression pushes them, a MOVE's numeric sender, a relation's operands.
Binary items are always valid. Unchecked, nothing is emitted. The other
statements that read numeric content do not check yet.

And the class test it rests on had a hole: IS NUMERIC on a packed item
was always true. cob_class now checks the digit nibbles (0-9) and the
sign nibble (A-F); GnuCOBOL agrees (free/packedclass).

Tests: 2002/ecincompat (ADD), 2002/ecincompat2 (COMPUTE),
free/packedclass (the oracle agrees, default dialect). Harness 446/446,
CCVS-85 unchanged, -std=85 byte-identical on all 229 Open Systems
programs, majesty PASS, majesty-functions PASS, Open Systems paper
unchanged. The per-statement rules held; the page is
docs/conformance/arithmetic.md, and the sweep's probe became
free/arithrules (the oracle agrees). Harness 447/447.

### 102. The INSPECT, STRING and UNSTRING sweep (2026-09-29)

The three statements against 2023 14.9.22, 14.9.43, 14.9.48 and the 85
text; the page is docs/conformance/string.md. CCVS-85 tests what must be
accepted, so the syntax rules were probed for what must be refused.

Found and fixed:
- operands the text forbids were accepted. INSPECT: a COMP inspected
  item or operand, a group operand, ALL figuratives, numeric literals.
  STRING: numeric literals, ALL figuratives, COMP items, a non-integer
  or P numeric sender. UNSTRING: a numeric (display) sender, numeric
  delimiters and DELIMITER IN items, edited, COMP and P receivers.
  Each is refused now, citing the edition compiled (insp_operand,
  str_operand, unstr_alnum, unstr_receiver).
- the POINTER's size (one more than the receiver's, or the sending
  item's, length) was checked by neither statement, and STRING's
  POINTER written without WITH skipped the integer check.
- the INSPECT tally had to be an integer; rule 5 asks only for an
  elementary numeric item.
- a POINTER of 0 was taken as "no POINTER": cob_str_begin and
  cob_unstr_begin read 0 as absent and started at position 1. The
  compiler now passes 1 when there is no POINTER, so 0 is the overflow
  (nothing moves, the POINTER keeps its value); the national entries
  also stopped setting an out-of-range POINTER to 1.

Tests: bad/inspect-operands, bad/string-operands, bad/unstring-operands;
free/inspectrules and free/stringrules (the oracle agrees on every
line). Harness 452/452, CCVS-85 unchanged, -std=85 byte-identical on the
227 Open Systems programs that compile, majesty PASS, majesty-functions
PASS, Open Systems paper unchanged.

### 103. EC-DATA-INCOMPATIBLE at every numeric sending reference (2026-09-29)

ISSUES-101 raised it in the arithmetic statements, MOVE and relations.
2023 14.6.13.2 rule 2 covers any statement that references a numeric
sending item's content, and the probes (template program, invalid
"1a3" in a PIC 9(3), checking on) found it silent in DISPLAY, STRING,
subscripts, reference-modification start and length, SET TO / UP BY,
PERFORM TIMES and VARYING FROM/BY, GO TO DEPENDING, function
arguments, INITIALIZE REPLACING ... BY, CALL BY VALUE and STOP RUN n.

Fixed where the value is read, not per statement where that is shared:
emit_expr_tokens, now emit_expr (every deferred expression: reference modification,
function arguments, expression operands) and emit_fn_value_raw (a
function's pushed arguments) test each operand as it is pushed; a
subscript item is tested in emit_ref_addr before it is loaded (the
running offset lives in r11, which the runtime keeps); the rest at
their statements. Several of those sites decode small DISPLAY integers
inline (opnd_hot_int), so the test comes before that branch. Nothing is
emitted unless the condition is checked, so unchecked code is
unchanged: -std=85 byte-identical on the Open Systems programs.

Not raised, by the rule's own exceptions or because the content is not
sent as a number: a class condition, UNSTRING's receivers, INSPECT of a
numeric item (examined as characters, not sent as a number), BY
REFERENCE and BY CONTENT arguments.

Rule 1 too: a boolean item of usage display or national whose content
fails the BOOLEAN class test (cob_class kind 4) raises it wherever
emit_incompat is reached -- MOVE, relations, DISPLAY, STRING -- and
boolean expression operands and shift counts (bool_emit_operand), which
were not. A USAGE BIT item is always valid. Not raised in a class
condition.

The condition is fatal, so one program cannot show more than one site:
harness gate 6 (tests/ecsites) builds tests/ecsites/template.cbl once
per line of sites.txt -- 30 sites that must raise, 8 that must not
(class tests, a binary subscript expression, an UNSTRING receiver, a
binary BY VALUE, bit operands, a boolean receiver). Harness 453/453.

### 104. INITIALIZE's COBOL 2002 phrases, and its syntax rules (2026-09-29)

WITH FILLER, `{ALL | category} TO VALUE`, THEN REPLACING and THEN TO
DEFAULT (2023 14.9.20) were refused as not implemented. A new walk,
init_walk / init_elem2k, visits every elementary item below
identifier-1 in order, every occurrence, and decides per item by GR 5-6:
the VALUE clause's value when the VALUE phrase names its category (NULL
for a pointer), else the REPLACING value, else the category default when
TO DEFAULT is given or neither VALUE nor REPLACING is. FILLERs only WITH
FILLER; REDEFINES items below the receiver never. The categories add
NATIONAL-EDITED and DATA-POINTER (a SET). The 1985 forms keep their own
path: -std=85 byte-identical.

Refusals added: a RENAMES item (85 rule 6; 2023 rule 5), an index-name,
a category repeated in REPLACING (85 rule 3; 2023 rule 6), the phrases
under -std=85. The 85 rule against OCCURS DEPENDING ON in identifier-1
(rule 4) would refuse majesty's gl008, gl034 and gl040, so it is a class
E point, BP-E15, taken and reported under -warn-extensions.

GnuCOBOL (the oracle) agrees on every case in 2002/init2002, and departs
from the text in two places, both in 2002/init2002cat (no oracle): it
restores every VALUE under `category TO VALUE` whatever the category
named (GR 5c1), and leaves a pointer alone under TO VALUE (GR 6a1).

Found on the way, not fixed here: a REDEFINES of an item with an OCCURS
clause is accepted (85 REDEFINES syntax rule 5); GnuCOBOL warns. That
is the next sweep.

With it, ALLOCATE data-name INITIALIZED (2023 14.9.3.4 GR 7: as
INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT), refused until now,
runs the same walk over the new storage when there was storage to be
had: 2002/allocinit, the oracle agrees; bad/std2002-allocate-based-
initialized removed.

Tests: 2002/init2002 (oracle agrees), 2002/init2002cat,
bad/initialize-operands, warn/ext-every (BP-E15), 2002/allocinit. Harness 456/456,
-std=85 byte-identical on the Open Systems programs, majesty PASS, Open
Systems paper unchanged. Page: docs/conformance/initialize.md.

### 105. The REDEFINES sweep (2026-09-29)

GnuCOBOL warned on a test (INITIALIZE, ISSUES-104) that REDEFINEd an
item with OCCURS, which we had accepted. The clause's syntax rules,
probed one by one, were mostly unchecked: the redefined item was found
by searching back for any earlier entry of that name and level.

Now data-name-2 must be the entry just before at its level, or the one
that entry redefines (2023 13.18.44.3 rules 4, 7, 10; 85 rules 8, 10,
11), and after layout a pass over the entries with a REDEFINES clause
(redef_clause: file records and SAME RECORD AREA share storage the same
way without one) refuses an OCCURS on data-name-2 or an ODO table on
either side (rule 5), a larger redefinition of anything but a non-
EXTERNAL level 01 item (rule 8; 85 rule 6), a VALUE in the entry or
below it but at level 88 (rule 9), and pointer items (rules 12, 14).
REDEFINES on a level 01 in the FILE SECTION is refused (rule 3). The
level-mismatch message named the redefined item as if it were the
subject; it names both now.

Nothing in CCVS-85, the Open Systems suite or majesty trips any of
these. Tests: bad/redefines-rules, -occurs, -larger, -value, -fd,
bad/std2002-redefines-pointer. Harness 462/462, -std=85 byte-identical
on the Open Systems programs, majesty PASS. Page:
docs/conformance/redefines.md.

### 106. The RENAMES sweep (2026-09-29)

2023 13.18.45.3 and 85 5.11.3, probed rule by rule. Found and fixed:
THRU naming data-name-2 again was accepted (rule 4); a data-name-3
inside data-name-2, or ending before it ends, was accepted -- only one
ending before data-name-2 began was refused (rule 11; 85 rule 8); a bit
item starting or ending the range inside a byte was not checked (rule
10); a strongly-typed item inside the range, not at its ends, was not
checked (rule 8). Naming the record itself or a level 77 item said "'g'
is not declared under 'g'"; it now cites rule 5 (85 rule 4), without
misfiring when the record also holds an item of that name.

GnuCOBOL refuses data-name-2 inside data-name-3 (b1 THRU b, b1 the
first item of b) because b is declared first; the text asks only about
the storage, so it is accepted here: free/renames3, no oracle.

Tests: bad/renames-same, -range, -level; free/renames2 (the oracle
agrees), free/renames3. Harness 467/467, -std=85 byte-identical on the
Open Systems programs, majesty PASS. Page: docs/conformance/renames.md.

### 107. The VALUE clause sweep (2026-09-29)

13.18.63 formats 1 and 3 and 85 5.15, probed rule by rule. The checks
that were there lived in the image builder and covered only what it
could not store; a new pass after layout (value_rules, recovering per
entry like init_record) takes the rest.

Refused now, accepted before: VALUE SPACES (any figurative but ZERO) on
a numeric item; a literal losing nonzero decimal digits; a signed
literal for an unsigned item (the sign was dropped); a group literal
longer than the group (truncated); a VALUE below a group that has one;
a JUSTIFIED, SYNCHRONIZED or non-DISPLAY item below a group with a
VALUE; for condition-names, literals that do not suit the conditional
variable (category, PICTURE range, size) and a THRU range running
downward; under -std=85, a VALUE on anything but a condition-name in
the FILE or LINKAGE SECTION. Kept: a nonnumeric literal of digits for a
numeric item, which CCVS-85 uses (NC107A, NC108M).

New: the level 88 FALSE phrase ([WHEN SET TO] FALSE IS literal) and SET
condition-name TO FALSE, refused as "not in COBOL 85" even under
-std=2002 before; rule 27 (the FALSE literal not among the values) and
SET rule 7 (the phrase is needed). 2002/condfalse, the oracle agrees.

Not done: 2023 rule 6, a numeric literal for a numeric-edited item
(edited at compile time as a MOVE would); the message says so.

Our own test fixed/tables had an 88 VALUE 50 THRU 100 on a PIC 99 --
by the rule 100 is out of range -- and now says 99. CCVS-85, the Open
Systems suite and majesty trip none of the new rules. Tests:
bad/value-rules (11 errors in one program), bad/std2002-value-false,
2002/condfalse. Harness 470/470, -std=85 byte-identical on the Open
Systems programs, majesty PASS. Page: docs/conformance/value.md.

### 108. The OCCURS sweep (2026-09-29)

13.18.38 formats 1-2 and 85 5.8. The KEY names were stored for SEARCH
ALL and never resolved: an undeclared key, one outside the table, one
with an OCCURS of its own or inside a nested table, a boolean key were
all accepted, and a qualified key (KEY IS k OF t) was stored as three
keys. A pass after layout, occurs_rules (recovering per entry), now
resolves each; the first scan stopped at the INDEXED BY names, which are
entered among the table's subordinates, and refused majesty's keys --
caught by the gates before commit.

Also refused now: an ODO table below a table (rule 1b); OCCURS m TO n
with n not above m (rule 16; 85 rule 5); an index-name as an operand of
anything but SET, SEARCH, PERFORM VARYING, a relation or a subscript
(rule 7; 85 rule 13), which had let ADD 1 TO i and DISPLAY i through.
Our 2002/bitarray2 displayed an index-name; it now SETs an integer.

Tests: bad/occurs-rules (5 errors), bad/index-name-operand;
bad/initialize-operands (the index-name message is the general one
now). Harness 472/472, CCVS-85 unchanged, -std=85 byte-identical on the
Open Systems programs, majesty PASS. Page: docs/conformance/occurs.md.

### 109. JUSTIFIED, SIGN, SYNCHRONIZED, level-numbers (2026-09-29)

The small data clauses, probed rule by rule. Refused now, accepted
before: JUSTIFIED on a group (both editions' rule 1) or on an edited
item (85 rule 3; 2023 rule 3's categories); under -std=85, SYNCHRONIZED
on a group (85 5.13.3 rule 1; 2002 allows it) and SIGN on a group with
no signed numeric DISPLAY item below it (85 5.12.3 rule 1; 2023 allows
it). The CODE-SET rule (signed items SIGN SEPARATE) was in place
already. clause_rules_one runs in the OCCURS pass.

Tests: bad/clause-rules (4 errors). Harness 473/473, CCVS-85 unchanged,
-std=85 byte-identical on the Open Systems programs, majesty PASS.
Page: docs/conformance/clauses.md.

### 110. The file control entry and FD sweep (2026-09-29)

12.4.5, 13.4.5 and the FD clauses, with the 85 I-O modules. Refused
now, accepted before: ACCESS RANDOM or DYNAMIC on a sequential file;
RECORD KEY on a file that is not indexed; an ALTERNATE RECORD KEY
beginning where another key does; a FILE STATUS item in a table or in
the FILE SECTION; a second FD for one file; DATA RECORDS naming no
record of the FD; RECORD CONTAINS m TO n with a record shorter than m,
or n not above m (that gave a confused message); a RECORD VARYING
DEPENDING ON item inside the record or signed; a signed or table LINAGE
data-name; a LINAGE footing beyond the page body. Under -std=2002, a
LINE SEQUENTIAL file with RESERVE, BLOCK or RECORD CONTAINS. An FD with
no record description said "file 'f' has no FD"; it cites the rule now
(and under -std=2002 says the record-less form is not implemented).

Two rules are extensions, not refusals: a numeric RECORD KEY (BP-E16;
the Open Systems suite has 13) and a numeric FILE STATUS (BP-E17), both
of which GnuCOBOL takes.

Tests: 12 bad/fd-* and bad/select-* programs,
bad/std2002-lineseq-clauses, warn/ext-every (BP-E16, E17). Harness
486/486, CCVS-85 unchanged, -std=85 byte-identical on the Open Systems
programs, majesty PASS, Open Systems paper unchanged. Page:
docs/conformance/files.md.

### 111. The I-O statements sweep (2026-09-29)

OPEN, CLOSE, READ, WRITE, REWRITE, DELETE, START against 2023 and the
85 I-O modules. Refused now, accepted before: OPEN EXTEND on a LINAGE
file or in random/dynamic access; NO REWIND on a non-sequential file or
with I-O; CLOSE REEL/UNIT/NO REWIND on a non-sequential file; READ INTO
the file's own record area; (2002) READ INTO a non-alphanumeric item
when the file has several records; END-OF-PAGE with ADVANCING PAGE;
INVALID KEY on REWRITE or DELETE of a relative file in sequential
access; OPEN INPUT or I-O of a report file. END-OF-PAGE without LINAGE was a parse error ("'at' is not a
COBOL verb") and cites the rule now.

A bug: WRITE ... AFTER ADVANCING 1 on an indexed file slipped through,
because the check tested the computed newline count (zero for AFTER 1)
rather than whether ADVANCING was written.

The 85 rule that AT END / INVALID KEY is required when no USE procedure
applies is BP-E18 (the Open Systems suite omits it 8 times, relying on
FILE STATUS). READ PREVIOUS is not implemented and now says so.

Tests: bad/io-rules (9 errors), bad/open-report-input, warn/ext-every
(BP-E18). Harness 488/488, CCVS-85 unchanged, -std=85 byte-identical on the Open Systems
programs, majesty PASS, Open Systems paper unchanged. Page:
docs/conformance/io-statements.md.

### 112. The SORT and MERGE sweep; SORT of a table (2026-09-29)

Refused now, accepted before: a SORT or MERGE inside a SORT's or
MERGE's input/output procedure (checked when the procedure division is
done, over the procedures' paragraph ranges) or in a declarative; a
boolean or pointer key; USING records longer than the sort record;
a sort record longer than a fixed-length GIVING file's; an indexed
GIVING file whose RECORD KEY is not the first, ascending key; a file
named twice in a MERGE; a random-access relative or indexed USING file.
A key in a table now cites the rule instead of asking for a subscript.

Our free/sortfile test sorted 30-byte input into a 29-byte SD; the SD is
30 bytes now. Nothing else in the corpora breaks the size rules.

New: SORT of a table (2002; 2023 14.9.40 format 2), which was refused.
cob_sort_table builds the file sort's normalized key per occurrence
(occurrence number trailing), merge-sorts them with memcmp and moves the
occurrences once. Keys default to the OCCURS clause's KEYs (rule 15);
an ODO table sorts its current count. GnuCOBOL agrees on 2002/sorttable
(no ties); with WITH DUPLICATES it reorders equal keys, which GR 3c
forbids, so 2002/sortdups has no oracle. A table inside another table
is not implemented.

Tests: bad/sort-rules (6 errors), bad/std2002-sort-table (5),
2002/sorttable, 2002/sortdups. Harness 492/492, CCVS-85 unchanged,
-std=85 byte-identical on the Open Systems programs, majesty PASS, Open
Systems paper unchanged. Page: docs/conformance/sort.md.

### 113. The SEARCH sweep (2026-09-29)

SEARCH ALL's WHEN was taken in any form, and a form the binary search
could not use was scanned. Format 2 allows one: one WHEN, KEY = value or
a single-valued condition-name, joined by AND, the key first, each KEY
subscripted at the table's level by its first index without + or -,
the values neither KEYs nor indexed by it, the keys a leading run of the
KEY list, and a KEY phrase on the table at all. sa_validate refuses
everything else. Its first cut asked for the index as the only
subscript and refused CCVS-85's nested tables (NC233A, NC237A, NC238A);
it checks the table's own level now. Also refused: NEXT SENTENCE with
END-SEARCH.

General rules: EC-RANGE-SEARCH-INDEX, registered and never raised, is
raised by a serial SEARCH whose index is outside the table at the
start (then AT END). GnuCOBOL departs from the text twice, both in
free/searchvary (no oracle): VARYING an integer item is set from the
index instead of incremented from its own value (GR 3b2), and an index
past the table searches from the first occurrence instead of ending
(GR 4).

Tests: bad/search-rules (10 errors), free/searchrules (the oracle
agrees), free/searchvary, 2002/ecsearchidx. Harness 496/496, CCVS-85
unchanged, -std=85 byte-identical on the Open Systems programs, majesty
PASS. Page: docs/conformance/search.md.

### 114. The EVALUATE and IF sweep (2026-09-29)

EVALUATE, refused now and accepted before: a THRU range of two classes
(1 THRU "z"), a literal subject with a literal object, a WHEN with no
statement after it. Given the rule instead of a parse error: a WHEN
with too few or too many objects, a condition or TRUE/FALSE object
under an identifier subject, a WHEN after WHEN OTHER, a partial
expression (COBOL 2014, not implemented). The first cut of the literal
rule refused CCVS-85 IF115A, whose subject FUNCTION LENGTH("...") the
compiler folds to a literal; the rule now looks at how the subject was
written.

IF, refused now and accepted before: no statement after the condition
or after ELSE; NEXT SENTENCE with END-IF.

Tests: bad/evaluate-rules (8 errors), bad/if-rules (3). Harness
498/498, CCVS-85 unchanged, -std=85 byte-identical on the Open Systems
programs, majesty PASS. Page: docs/conformance/evaluate.md.

### 115. ACCEPT, DISPLAY, GO TO, ALTER; 2002's deleted elements (2026-09-29)

Refused now, accepted before: a statement after an unconditional GO TO
(never reached); a paragraph named by ALTER that is not one sentence of
a GO TO without DEPENDING (85 ALTER rule 1); ACCEPT of DATE, DAY, TIME
or DAY-OF-WEEK into an alphabetic or boolean item. An undeclared
mnemonic-name after ACCEPT FROM or DISPLAY UPON said "not implemented";
it cites the rule now.

New: ACCEPT ... FROM DATE YYYYMMDD and DAY YYYYDDD (2002), a parse
error before (cob_accept_datetime cases 4 and 5); 2002/acceptyyyy, the
oracle agrees.

Under -std=2002 the elements 2002 deleted (F.1) -- ALTER, comment-
entries, STOP literal, OPEN REVERSED, MEMORY SIZE, LABEL RECORDS, VALUE
OF, DATA RECORDS, RERUN, MULTIPLE FILE TAPE -- were accepted with at
most a -warn-74 warning. bp() now refuses BP-O1 to BP-O11 under
-std=2002, except BP-O9, which 2023 permits again (our 2002/movecorr
uses it). Nothing changes under -std=85.

Tests: bad/goto-alter, bad/accept-rules, bad/std2002-deleted,
2002/acceptyyyy (+ .env). Harness 502/502, CCVS-85 unchanged, -std=85
byte-identical on the Open Systems programs, majesty PASS, Open Systems
paper unchanged. Page: docs/conformance/accept.md.

### 116. READ PREVIOUS (2026-09-29)

The I-O sweep (ISSUES-111) left READ PREVIOUS (COBOL 2002) as a named
gap. Implemented for indexed and relative files by 2023 14.9.30.4 GR 21:

- indexed (idx_read_prev): after OPEN the at end condition; after START
  the record START found; after a READ the record before the one it
  delivered. Each is "the last entry on the key of reference below a
  bound", the bound including the cursor's entry after START (cob_idx
  cur_at, new) and excluding it after a READ (the cursor is one past
  it). The record delivered leaves the cursor one past it, so NEXT and
  PREVIOUS continue from it either way; duplicates on an alternate key
  come back in reverse arrival order, status 02 while more precede.
- relative (rel_read_prev): after OPEN or START the record NEXT would
  give; after a READ the first existing record with a lower number.

Refused: with a KEY phrase, on a LINE SEQUENTIAL file (rule 7), in
random access (rule 6); on a sequential file it is not implemented.

GnuCOBOL agrees on every indexed case except the first: READ PREVIOUS
straight after OPEN is status 46 there, where GR 21d3 says at end. Its
relative READ PREVIOUS goes wrong twice (from record 4 it skips an
existing record 2; at a missing record it reports at end), so those
cases are in 2002/readprev2 without an oracle.

Tests: 2002/readprev (the oracle agrees), 2002/readprev2. Harness
505/505, CCVS-85 unchanged, -std=85 byte-identical on the Open Systems
programs, majesty PASS, Open Systems paper unchanged.

A regression found on the way: ISSUES-110 refused RESERVE, BLOCK and
RECORD CONTAINS on a LINE SEQUENTIAL file under -std=2002, and majesty's
jerm (built -std=2002 by tests/majesty-functions.sh) has RECORD CONTAINS
on one. That gate was not run for ISSUES-110 to -115, so jerm failed to
build from c8ff2f4f until here. It is BP-E19 now, a warning under
-warn-extensions; the warn gate runs *std2002* files under -std=2002
(warn/ext-std2002-lineseq replaces bad/std2002-lineseq-clauses).
tests/majesty-functions.sh PASS again.

### 117. Thirty-one digits (2026-09-29, in progress)

COBOL 2002's limit for a numeric item and literal is 31 digits; s32-cobc
has held 18 (a 64-bit integer and a scale). The plan is docs/wide.md: a
wide path beside the narrow one, chosen at compile time from the
descriptors, so everything that fits 18 digits keeps today's code
(-std=85 byte-identical) and only 19-31-digit items, literals and
statements take it. Three phases: items and moves; arithmetic; the rest
(functions, edited receivers past 18, BINARY-DOUBLE, keys, class test).

Foundation: libcob/wide.h -- a sign, a scale and a 128-bit magnitude in
four 32-bit limbs (SLOW-32's clang has no __int128), with multi-limb
add, subtract, compare, small multiply and divide, full multiply, long
division, and decimal conversion. tests/wide_test.c checks every
operation against the host's unsigned __int128 on 200,000 random
operand pairs and every pair of 37 edge values (limb boundaries, powers
of ten); it is harness gate 1c. Mutation-checked: a borrow bug survived
the random operands alone, which is why the edges are there.

Phase 1 (items and moves), done. Runtime: cob_wget and cob_wput_x, the
wide cob_get_num and cob_put_num_x (DISPLAY with every sign form,
BINARY of sixteen bytes little-endian two's complement, PACKED-DECIMAL,
numeric-edited through cob_deedit and cob_edit_apply, national through
the narrow copy), with ROUNDED and the size-error report ready for
phase 2. cob_move, cob_cmp, cob_display_field and num_to_digits take
them when a descriptor passes 18 digits; cob_put_num_x hands a wide
receiver to cob_wput_x; cob_get_num on a wide item returns its value
when it fits 64 bits and stops the run when it does not -- never a
wrong number. Compiler: pic_max_digits (31 under -std=2002), BINARY of
19-31 digits is sixteen bytes, VALUE of such items (the compiler
includes wide.h too), literals of 31 digits under -std=2002,
numlit_int and numlit_scaled saturate instead of wrapping, and
arithmetic touching a wide operand or receiver is refused by name
until phase 2.

Tests: 2002/wide1 (25 lines, the oracle agrees: DISPLAY, BINARY and
PACKED-DECIMAL of 20-31 digits, signs, fractions, truncation both
ends, edited and alphanumeric sides, relations across widths and
scales), bad/std2002-wide-limits. Harness 508/508, -std=85
byte-identical on the Open Systems programs, majesty PASS,
majesty-functions PASS, Open Systems paper unchanged.

Phase 2 (arithmetic), done. Runtime: a wide evaluation stack -- cob_wpush,
cob_wpush_lit, cob_wadd, cob_wsub, cob_wmul (the full product in eight
limbs, fraction digits shed until it is below 10^38), cob_wdiv (the
operands' larger scale and six guard digits, at least nine, truncated; a
256-bit numerator), cob_wneg, cob_wtrunc, cob_wpow (repeated
multiplication), cob_wcmp, cob_wtop_store and the ADD TO / SUBTRACT FROM
forms, cob_wdrop, cob_wpop_int / cob_wpop_pos -- sharing div0 and
size_kind with the narrow stack, so ON SIZE ERROR and EC-SIZE behave the
same. Compiler: g_wide, set per statement when an operand, literal or
receiver passes 18 digits or (under -std=2002) the composite of operands
does, turns emit_call's stack operations into their wide names and
turns off the inline fast paths (hot binary sums, the decimal ADD, the
DISPLAY-integer push); COMPUTE learns it from a pass that emits nothing,
a condition's expression operands carry it (Opnd.wide), REMAINDER and
the VARYING / SET step decide for themselves. It is cleared before ON
SIZE ERROR's statements, which decide their own. A function in wide
arithmetic is refused until phase 3.

Tests: 2002/wide2 (22 lines, the oracle agrees: each statement,
ROUNDED against truncation, ON SIZE ERROR, REMAINDER, a product of two
18-digit items into 31, a composite of 19-31 from narrow items,
relations on expressions, VARYING a wide item), 2002/wide3 (powers of
two to 2 ** 110, 10 ** 30, a negative base; no oracle -- GnuCOBOL gives
zero for 2 ** 90 into S9(31)), bad/std2002-wide-limits (a composite of
32). Harness 510/510, -std=85 byte-identical on the Open Systems
programs, majesty PASS, majesty-functions PASS, Open Systems paper
unchanged.

A randomized differential, tests/wide-differential.sh (tests/wide-gen.py):
COMPUTE statements over random items of 1-31 digits, every usage, random
scales and signs, + - * with one level of parentheses, into random
receivers, ROUNDED or not, each line its value or its size error. 960
statements over twelve seeds agree with GnuCOBOL line for line. The first
run found 34 differences, every one a BINARY receiver of more than 18
digits whose result overflows its PICTURE: we report the size error, as
the narrow path does for BINARY; GnuCOBOL checks the sixteen-byte field
only and stores a truncated value. That case is left out of the script
and noted in it. Division is left out too (the intermediate precision is
the implementor's).

Phase 3, done. BINARY-DOUBLE [SIGNED | UNSIGNED] (U_SDBL, U_UDBL: eight
bytes, 20 digits shown as GnuCOBOL shows them, the field's capacity the
size-error limit -- cob_wput_x checks a native binary's capacity now),
which was refused by name. The exact intrinsic functions moved to the
wide stack (cob_fn_wnum) with results described at run time
(cob_fn_var_desc kind 3; Opnd.fwnum; a function's arguments go to the
stack its implementation reads, whatever the statement around it uses),
and NUMVAL, NUMVAL-C and NUMVAL-F scan into a wide value. SORT keys past
18 digits get a sixteen-byte normalized key.

This fixed a bug older than the 31-digit work: numeric function results
were an S9(9)V9(9) buffer, and in a plain COBOL 85 program MAX, MIN,
SUM, RANGE, MIDRANGE and NUMVAL of a value past nine integer digits came
back as garbage (MAX of 123456789012 was 223372036), silently.
free/fnwidth (the oracle agrees). A result that fitted the old buffer is
written exactly as before, so DISPLAY of function values is unchanged.

Tests: 2002/bindouble, 2002/wide4 (30 lines, the oracle agrees),
free/fnwidth. Harness 512/512, CCVS-85 unchanged, -std=85 byte-identical
on the Open Systems programs, majesty and majesty-functions PASS, Open
Systems paper unchanged, wide-differential agrees. Not done: the
floating functions keep their double-precision S9(9)V9(9) result
(docs/wide.md).


### 118. The DATA DIVISION sweep (2026-09-30)

docs/conformance/data-division.md: the sections, the data description
entry, FILLER, EXTERNAL, GLOBAL, CODE-SET, LINKAGE. Rules that were
accepted and are now refused, each with a test:

- the sections out of order, or one twice (85 IV-34): bad/section-order,
  bad/section-twice;
- REDEFINES after another clause; BLANK WHEN ZERO on a group; a level 77
  entry with no name (85 VI-18, VI-21): bad/entry-rules;
- a condition-name on a level 66 entry, which was refused with a VALUE
  length message instead: bad/condname-66;
- EXTERNAL below level 01, in LINKAGE, on FILLER, with REDEFINES or
  BASED, twice under one name, and under 85 with a VALUE (X-21, X-23):
  bad/external-rules;
- GLOBAL below level 01, on a 77, in LINKAGE under 85, on FILLER, twice
  under one name (X-21, X-24): bad/global-rules; and on a file in a SAME
  RECORD AREA: bad/global-sra;
- under -std=85, a LINKAGE item that is not a USING operand, nor under or
  redefining one (X-25, rule 4): bad/linkage-ref;
- TYPEDEF after another clause (2002 13.13.2 and 2023 13.16.3, rule 4:
  immediately after the name). Two of our own tests had TYPEDEF last,
  as GnuCOBOL accepts it: 2002/typedecl and bad/std2002-typedef-strong,
  corrected.

Messages that now name the rule or the feature: an elementary item
without a PICTURE (bad/no-picture), a group with one (bad/group-picture);
the 2002 features not implemented -- the constant entry, EXTERNAL AS, a
PICTURE implied by VALUE, SAME AS, ANY LENGTH, LOCALE -- and the 2014
ones, CONSTANT RECORD and DYNAMIC LENGTH, which were "unexpected 'x'".

One rule became a behavior point instead: 85 general rule 2c (2023 rule
24c and d) keeps a condition-name off a group holding COMP, PACKED,
JUSTIFIED or SYNCHRONIZED items. Majesty's records carry `88 ... VALUE
HIGH-VALUES` end-of-file flags over packed fields (crgltrans and the
copybook test failed when it was refused), so it is BP-E23: taken,
warned under -warn-extensions. The user's ruling (2026-09-30): keep the
point, and change majesty to conform -- majesty 1dfaa92 moves both 88s
onto a PIC X record of the same length; the gltrans trio is
byte-identical.

Open: a function's formal parameter used as a receiving operand (2002
and 2023 13.7.3 rule 5) is accepted. Receiving operands are parsed by
each statement, with no common point to check them; a function that
changes its parameter changes its caller's argument.

### 119. Three defects found by porting majesty's csv2fw (2026-09-30)

Dogfooding (moving the month-end's last host steps onto SLOW-32) put a
real program -- a byte-level CSV state machine with reference
modification everywhere -- through the compiler, and three defects fell
out, none of which the harness, CCVS-85 or the generators had reached:

- **A reference-modification start with a subscripted operand was
  late.**  `r(p + w - len(i):n)`: addressing `len(i)` uses r11, the
  register the outer item's offset accumulates in, and left its own
  offset there, so the start came out (i - 1) times len's size too far.
  The start is now evaluated before the outer offset begins, and waits on
  the numeric stack (emit_expr_pos_push/_pop).  Test: free/refmodsub.
- **A string function's reference-modified argument was the item from
  the start position to its end.**  `NUMVAL(t(p:1))` read t from p on, so
  "2023" scanned a digit at a time gave 2023, 23, 23, 3; the same for
  NUMVAL-C, NUMVAL-F, TEST-NUMVAL, REVERSE and the list functions'
  arguments.  They take the modification's length now
  (emit_ref_addr_len), and REVERSE's result is that length, refused by
  name when it is known only at run time, as UPPER-CASE's is.  Test:
  free/fnargrm.
- **DISPLAY UPON SYSERR went to stdout.**  It, UPON STDERR, and a
  mnemonic-name for SYSERR now write to stderr, as GnuCOBOL, IBM and
  Micro Focus do; the compiler looks ahead to UPON before any operand is
  written.  Test: free/syserr.  Majesty's gl038 sends its error lines
  there, and now they arrive there.

The port itself (majesty src/cobol/csv2fw.cbl) matches the C++ byte for
byte on the real exports and on 600 random trials (majesty
tests/csv2fw/differential.sh).

### 120. The X-COBOL compile survey (2026-10-01)

Third-party COBOL, none of it written for us: X-COBOL (Zenodo
10.5281/zenodo.7968845, CC-BY-4.0), 844 .cbl/.cob files from 84 GitHub
projects, kept in ~/refs/x-cobol and never in a git tree.
tests/xcobol-survey.py compiles each in the format its text declares,
-std=85 then -std=2002, and records the first error.  Before the fixes
below 154 compiled (131 before the survey stopped blaming its own
format guesses and flattened copybook paths on the compiler); after
them 173, and all 14 of a GnuCOBOL sample repository's programs.

Fixed:

- **END-DISPLAY was never consumed** (2023 14.9.11.2: every format of
  DISPLAY takes it), so the next statement found it "without a matching
  statement".  Accepted under -std=2002, refused by name under 85.
  Tests: 2002/enddisplay, bad/end-display-85.
- **END-PROGRAM as a paragraph-name: "internal: paragraph not
  prescanned".**  It is reserved in no standard but sat in the
  scope-terminator list; the prescan skipped it and the paragraph pass
  did not.  Out of the list, and the paragraph pass now applies the
  prescan's test, so a stray END-IF. says so instead of the internal
  error.  Test: free/endprogpara.
- **Arithmetic-expression subscripts** (2002 8.4.1.2.1) were not
  implemented: E(9 - I), E(I * 2), E(K(2)), K(I - N, 1) were parse
  errors under either standard.  A subscript the 85 forms cannot
  express is now an expression, evaluated before the reference's
  address is formed (its operands may themselves be subscripted, and
  addressing uses r11) and left on the numeric stack as an integer;
  under EC-BOUND-SUBSCRIPT checking a value that is not an integer
  raises it, as SET's does.  Under -std=85 it is refused by name.  A bit
  data item's expression subscript is refused as not implemented.
  Tests: 2002/subexpr (identical to GnuCOBOL), 2002/subexprchk,
  bad/subscript-expr-85.
- **( a OMITTED ) was taken for an arithmetic expression**: the
  parenthesis test knew the relational and class words but not OMITTED,
  BOOLEAN or a class-name.  Test: 2002/omitparen.
- **COPY looks for .cob/.COB too**, as GnuCOBOL does (a test framework
  in the dataset keeps its copybooks so).

Not defects -- the rest, by first error, as candidates for behavior
points if a program we care about needs one: Micro Focus $SET lines and
>>IF; level 78 and OCCURS at level 01/77; USAGE POINTER inside an
ordinary group (2002 USAGE rule 13 and 2023 13.18.60.3 rule 14 allow it
only at level 1 or under a STRONG type -- every IBM CICS sample breaks
it); IBM CBL/PROCESS cards and UT-S- assignment names; tabs in fixed
form; ASSIGN DYNAMIC/KEYBOARD, ACCEPT FROM ENVIRONMENT, a literal
PROGRAM-ID, INSPECT ... TRAILING.  Unimplemented 2002/2014 features met:
& literal concatenation, CONSTANT entries, FUNCTION TRIM.  About 90
programs break a standard rule we enforce (EXIT not alone, a period or
comma not followed by a space, an empty sentence); about 180 cannot
compile anywhere as shipped (copybooks named .cbl, JCL decks, CICS/DB2
copybooks and copybooks the dataset does not have).

Second batch, the same day:

- **The constant entry** (2002 13.9; 2023 13.10): `01 name CONSTANT [IS
  GLOBAL] AS` a literal, a compile-time arithmetic expression, `LENGTH
  OF` or `BYTE-LENGTH OF` a data-name.  General rule 1 says the effect
  is as if the literal were written where the name is, and it is done
  that way: when the entry is parsed, the rest of the program's tokens
  (a contained program's too, under GLOBAL) have the name replaced by
  the literal, and a PICTURE's `(name)` by the integer, so every place
  that takes a literal takes the constant unchanged.  LENGTH OF waits
  for the DATA DIVISION's layout: its tokens share a buffer filled then,
  and a use inside that DATA DIVISION is refused as not implemented (a
  contained program's is filled).  The arithmetic is exact fractions
  over literals of at most 18 digits, and a value that is not an
  integer is refused.  Not yet: constant entries in the REPORT and
  SCREEN SECTIONs, and `FROM` a compilation variable (no >>DEFINE).
  Tests: 2002/constent (identical to GnuCOBOL but for one documented
  divergence, `.oracle-expected`), 2002/constdup (no oracle: GnuCOBOL
  4.0-early-dev crashes on a constant-name defined twice the same way,
  which rule 9 allows), six bad/std2002-constant* tests, and
  bad/constant-85.  docs/conformance/data-division.md has the rules.
- **BP-E24, comment-entries under -std=2002**, the user's ruling: taken
  as comments, warned under -warn-extensions.  Seventeen X-COBOL
  programs carry AUTHOR or DATE-WRITTEN beside a 2002 feature, and were
  refused under either standard.  The other deleted elements stay
  refused under 2002.
- **BP-E25, `CONSTANT literal` without AS**, GnuCOBOL's form; taken.

The survey: 180 compile.  Of the programs the constant entry and the
comment-entries held back, seven compile; the rest stop later, mostly at
level 78, OCCURS at level 01, FUNCTION TRIM and ANY LENGTH.

Third batch: **level 78** (BP-E26), Micro Focus's constant-name, on the
constant entry's substitution, under either standard: a literal keeps its
class; anything else is an integer by MF's rules (strictly left to right,
64-bit, the bitwise AND, OR, EXCLUSIVE OR, NOT); LENGTH or SIZE OF a
literal (digits or characters) or of a data item (its storage, after
layout).  NEXT, START OF, DATE-COMPILED and the boolean values are
refused as not implemented.  Test free/level78 (no oracle: GnuCOBOL has
no EXCLUSIVE OR and refuses LENGTH OF a numeric literal; the rest agrees
under its -std=mf), bad/level78-next, bad/level78-divzero,
warn/ext-level78, and warn/ext-every.  The survey: 192 compile.

Fourth batch:

- **Concatenation expressions** (2002 and 2023 8.8.3): `literal &
  literal` is one literal (general rule 3), so the text is joined once
  COPY and REPLACE are done, before anything parses it, the >>TURN
  positions mapped past the tokens it removes.  SPACE, ZERO and QUOTE
  take the other operand's class (rule 1); HIGH-VALUE and LOW-VALUE are
  refused, their characters being the collating sequence's, which is not
  known then.  Refused by name under -std=85.  Tests 2002/concat
  (identical to GnuCOBOL), 2002/concatfig (no oracle: GnuCOBOL refuses a
  figurative operand), four bad/ tests.
- **FUNCTION TRIM** (2014; 2023 15.96) as BP-E27, under either standard,
  with 2023's characters to delete (literals), its result of run-time
  length and zero when nothing is left.  A run-time-length function as a
  STRING source was "not supported here yet"; the length now comes from
  the runtime beside the function's address (A_FLEN), for DISPLAY-OF and
  NATIONAL-OF too.  Tests 2002/trim (identical to GnuCOBOL),
  2002/trimchars (no oracle: GnuCOBOL has only the 2014 form), two bad/
  tests, warn/ext-trim, warn/ext-every; bad/std2002-intrinsic-2014 now
  names SECONDS-PAST-MIDNIGHT.

Fifth batch: **ANY LENGTH** (2002; 2023 13.18.2).  The caller leaves
each argument's length in bytes beside the count (cob_call_lens and
cob_call_nlens, set by every CALL and user-function invocation compiled
-std=2002); the called program puts it in the parameter's own writable
descriptor at entry, saved and restored with the activation's words when
it recurses; and every reference to the item becomes a reference
modification to its end, so each statement takes the run-time length as
it takes a computed part's.  An outermost program's ANY LENGTH is taken
as BP-E28; a caller compiled -std=85 or C passes no lengths, and the run
stops naming the program.  Two gaps closed on the way: FUNCTION
BYTE-LENGTH of a part with a computed length (counted at run time, as
LENGTH is), and a string function's argument with a computed length
(DISPLAY-OF, NATIONAL-OF, TRIM; it was "of known length" only).  Gaps
left: PICTURE 1, a program's RETURNING item, a zero-length argument.
Tests 2002/anylen (identical to GnuCOBOL), 2002/anylenfn (no oracle:
GnuCOBOL dies with SIGSEGV), 2002/anylennat, six bad/ tests,
warn/ext-std2002-anylen-outer.

Found while writing 2002/anylen (**fixed** the same day, below): a
contained program may CALL a sibling that is not COMMON.  GnuCOBOL refuses it at run time ("module
not found"), and the scope rules make only a directly contained program,
or a COMMON sibling, visible by name; this compiler links any program
the CALL names.

Sixth batch: **the scope of program-names** (2023 8.4.6.3).  A scan of
the tokens before anything is compiled builds the tree of programs with
COMMON and RECURSIVE (a containing program's CALLs are parsed before its
contained programs are).  A contained program's entry is now a local
symbol, .Lcp<n>: a literal CALL in scope jumps to it, and one out of
scope means an outermost program of that name, looked for at run time --
and the registry, where contained programs are marked, hands one out
only to a caller whose table of visible contained programs lists it
(CALL identifier, ON EXCEPTION, CANCEL, the EC-PROGRAM-RECURSIVE-CALL
check).  Test free/progscope (no oracle: GnuCOBOL takes ON EXCEPTION
after successful CALLs there, and dies with SIGSEGV calling an
out-of-scope active program).

Seventh batch: **INSPECT of a function's value**, which X-COBOL's
command-line parser writes (`INSPECT FUNCTION TRIM(args) TALLYING`), met
"'function' is not declared".  A function-identifier is an identifier
(2023 8.4.3.2), and TALLYING only reads its subject, so it is taken: the
function is evaluated and its value, at its run-time length, inspected.
REPLACING and CONVERTING would change it and are refused naming 8.4.3.2.3
rule 1 (not a receiving operand); a numeric function's value is refused
by INSPECT's rule 1.  Tests 2002/inspfunc (identical to GnuCOBOL), three
bad/std2002-inspfunc-* tests.

Eighth batch: **ROUNDED MODE** (2014; 2023 14.7.4) as BP-E29, all eight
modes, under either standard.  NEAREST-AWAY-FROM-ZERO is plain ROUNDED
and TRUNCATION is no ROUNDED, so both compile exactly as before; any
other mode sends its statement down the stack path (the register and
decimal fast paths are off for it) and travels in the store's opts bits
4-7.  The narrow store rounds the value to the receiver's scale by the
mode before cob_put_num_x, which is a DBT-hooked thunk left untouched
(the stored value is then exact); the wide store decides in its own
digit-dropping step.  PROHIBITED with an inexact value is the size error,
the receiver unchanged.  Tests 2002/rmode (GnuCOBOL raises PROHIBITED's
size error on an exact value; .oracle-expected), 2002/rmodewide
(identical), bad/rounded-mode (an unknown mode), warn/ext-rounded-mode.

Ninth batch, toward abrignoli_COBSOFT (a Micro Focus business system,
45 of whose programs the survey stopped at their first line):

- **The IDENTIFICATION DIVISION header is optional from 2002** (11.1.1
  shows it in brackets): a program, a contained one too, may begin at
  PROGRAM-ID or FUNCTION-ID.  Under -std=2002 it was "expected
  IDENTIFICATION DIVISION", a conformance defect; every place that finds
  a unit's start (the paragraph prescan, the PROCEDURE DIVISION's end,
  EXIT's sentence rule, the skip to the next sentence) now takes either.
  Under -std=85 a missing header is refused naming the edition.  Tests
  2002/noidhdr (identical to GnuCOBOL), bad/idhdr-85.
- **$SET directive lines** (BP-E30): SOURCEFORMAT"FREE"/"FIXED"
  switches the reference format as >>SOURCE does; listing directives
  have no effect; any other directive, and $IF/$DISPLAY lines, are
  refused by name rather than ignored.  A free-form line beginning `$$`
  is a picture going on.  Tests fixed/dollarset (identical to GnuCOBOL),
  bad/dollarset-unknown, bad/dollarset-if, warn/ext-dollarset.

Tenth batch: **-dialect=mf**, and its first point.  By the user's ruling
(2026-10-01) Micro Focus is the dialect a switch is for.  A new class of
behavior point, D: a dialect's own form, taken only under its switch and
refused without it naming the switch, warned under -warn-extensions as
class E is.  The class E points stay as they were, always taken.
**BP-D1**: file-control entries without the FILE-CONTROL header (MF's
reference brackets it), or without the INPUT-OUTPUT SECTION header as
well (not marked in the reference; MF practice, and abrignoli_COBSOFT's
29 programs, leave it out with the other).  The harness compiles a test
named mf-* under -dialect=mf, its oracle GnuCOBOL -std=mf.  Tests
free/mf-selectbare, free/mf-selectnofc (both identical to GnuCOBOL),
bad/select-nofc, warn/ext-mf-select.

Eleventh batch: **split keys**.  They turned out to be standard first:
2002 has `RECORD KEY IS record-key-name SOURCE IS data-name ...` (and
the ALTERNATE RECORD KEY's, 12.3.4.12), the key the parts' concatenation,
named by READ and START; Micro Focus's `name = data-name ...` is its
spelling of the same, parts of any category, BP-D2 under -dialect=mf.
Kept as docs/indexed.md describes: a tail on the record area, one slot
per split key, filled by the runtime from the parts before each keyed
operation, so the B-tree needs no change.  Variable-length records with
a split key are refused as not implemented.  Tests 2002/splitsrc and
free/mf-splitkey (both identical to GnuCOBOL), bad/splitkey-85,
bad/splitkey-nodialect, bad/std2002-splitkey-category,
bad/std2002-splitkey-move, warn/ext-mf-splitkey.  s32sort (dfsort/) reads
flat files only, and its multi-field SORT FIELDS is already a composite
key, so it needs nothing.

Twelfth batch: **BP-D3 and BP-D4**, two rules Micro Focus's reference
says it does not enforce -- STOP RUN as the last statement of its
sequence (what follows never runs) and EXIT alone in its sentence and
paragraph (such an EXIT does nothing).  abrignoli_COBSOFT ends its
subprograms `exit program stop run exit.`, which needs both.  Without
-dialect=mf the standard refusals stand, their messages unchanged.  Test
free/mf-stopexit (identical to GnuCOBOL -std=mf), warn/ext-mf-stopexit.

Thirteenth batch: **BP-D5**, the FILE SECTION header left out, an FD or
SD first in the DATA DIVISION.  Micro Focus's reference does not mark it
optional; abrignoli_COBSOFT's 29 programs show its practice, and by the
user's ruling (2026-10-01) it is taken under -dialect=mf as BP-D1's
section header is.  Tests free/mf-nofilesec (identical to GnuCOBOL
-std=mf), bad/fd-no-section, warn/ext-mf-nofilesec.

Fourteenth batch: **PIC X(8) COMP-5**.  Eight bytes in the machine's
order hold 2^64 - 1, twenty digits; that item is BINARY-DOUBLE UNSIGNED,
so it is taken as one, on the wide path -- -std=2002, and under -std=85
refused naming why.  COMP-X's eight-byte X picture (big-endian) stays
not implemented.  GnuCOBOL keeps 19 digits of it (losing the twentieth)
and stores it big-endian, -std=mf too (.oracle-expected,
docs/oracles.md).  abrignoli_COBSOFT's twelve programs needed it.  Tests
2002/comp5x8, bad/comp5-x8-85.

Fifteenth batch: **BP-D6**, ASSIGN TO a data-name declared nowhere.
Micro Focus's SELECT rule 4 declares it implicitly, alphanumeric and
long enough for a file name; under -dialect=mf it becomes a
WORKING-STORAGE 01 PIC X(1024) (the size a ruling: the reference leaves
it to the operating system, GnuCOBOL uses 4095), made before the
records are put together.  Without the switch it is refused naming it.
abrignoli_COBSOFT's 11 programs STRING a path into such an item before
the OPEN.  Tests free/mf-assignimp (identical to GnuCOBOL -std=mf),
bad/assign-undeclared, warn/ext-mf-assignimp.

Sixteenth batch:

- **A screen entry with PICTURE and VALUE** is standard 2002 (13.15.2
  rule 7, GR 3: the PICTURE "may be omitted" for an alphanumeric literal,
  so it may be written).  It was refused ("a VALUE slot takes no
  PICTURE").  The literal now fills a field of the picture's size,
  padded with spaces, or cut on the right with a warning; a numeric
  VALUE with a numeric PICTURE is refused as not implemented.
  abrignoli_COBSOFT draws its menus so (23 programs).  Test
  free/scrpicval.
- **BP-D7**, a VALUE literal longer than its alphanumeric item, the
  user's ruling: under -dialect=mf it is cut on the right (JUSTIFIED does
  not change initialization, 2023 13.18.63.4 rule 7 -- a first version
  cut a JUSTIFIED item on the left, and GnuCOBOL's oracle said
  otherwise) and always warned.  The harness's warn gate now asks only
  that no behavior point's warning appear without the flag, so an
  always-on warning can stand.  A new warn_at() gives such warnings.
  Tests free/mf-valtrunc (identical to GnuCOBOL -std=mf),
  bad/value-too-long, warn/ext-mf-valtrunc.

Seventeenth batch: **a positioned ACCEPT's screen clauses** (BP-E7).
`accept x at line 11 col 34 with update auto-skip` -- abrignoli_COBSOFT,
24 programs -- met "'auto-skip' is not a COBOL verb": the statement knew
UPDATE, PROMPT, SIZE and the like, not the screen entry's own clauses.
It now takes AUTO (AUTO-SKIP), SECURE, REQUIRED (EMPTY-CHECK), FULL
(LENGTH-CHECK), UNDERLINE, HIGHLIGHT and LOWLIGHT, with the SCREEN
SECTION's meanings, the input ones on ACCEPT only.  Test free/posauto
(keys typed with no Enter between the two fields).

Eighteenth batch: **reference modification in screen I/O**.  A
positioned ACCEPT into a part (`accept f-cpf(07:03) at line 11 col 42`,
abrignoli_COBSOFT keying a CPF number piece by piece), a positioned
DISPLAY of a part, and a SCREEN SECTION field FROM, TO or USING a part
were all "not implemented".  The
field now reads and writes the part: its address comes from the
reference as for any other (computed at ACCEPT time when the start is),
its descriptor is the part's (alphanumeric or national, of the part's
length), and its width the part's characters.  A part of computed length
is refused naming it.  Test free/posrefmod.

Nineteenth batch: **the environment** (BP-E31), X/Open's and Micro
Focus's: DISPLAY ... UPON ENVIRONMENT-NAME chooses a variable, ACCEPT ...
FROM ENVIRONMENT-VALUE reads it, DISPLAY ... UPON ENVIRONMENT-VALUE sets
it (trailing spaces kept, MF DISPLAY rule 9), ACCEPT ... FROM
ENVIRONMENT name reads one in a step; ON EXCEPTION / NOT ON EXCEPTION on
each.  The guest libc has no setenv, so a value set is kept in a table
the run unit reads before the environment.  Writing it, the behavior
point's table entry went in ahead of BP-D7 while its enum went after,
and the first test caught the swap.  GnuCOBOL runs neither EXCEPTION
phrase when the variable is present, and blanks the item on the
exception (MF: undefined; kept here).  Test free/envvar (.env,
.oracle-expected).

Twentieth batch: **a screen FROM literal** (2002 13.15.1: FROM
identifier-5 or literal-1).  `pic x(01) from "-"` met "expected a
data-name"; it is now the literal through the entry's PICTURE, the same
field as PICTURE with VALUE (padded, or cut with the always-on warning),
and refused without a PICTURE (rule 7).  A numeric literal is refused as
not implemented.  Tests free/scrpicval, bad/screen-from-lit-nopic.


### 121. The front-end pass (2026-10-01, in progress)

docs/plans/frontend-pass.md: parse a construct into a tree first, emit
from the tree, so the order of the code is the emitter's choice and not
the order the tokens were read in.  Each step is checked by
tests/asm-snapshot.sh (1477 programs) diffing empty against the step
before, beside the usual gates.

First, the compiler source was split into src/cobc/*.h, one
translation unit, the binary byte-identical (6bf79791).

Step 1: **expression trees for the stored token ranges.**  parse_expr
returns an Expr (a leaf Opnd, or an operator over subtrees) whether or
not it emits; expression subscripts, computed reference-modifier start
and length (of an item or of a function), O_EXPR operands and ALLOCATE's
size keep the tree, and emit_expr walks it where emit_expr_tokens
re-parsed the tokens.  emit_expr_pos_push read each expression twice
more -- once to learn whether it needed the wide stack, once to emit --
and now reads the width the scan recorded.  move_needs_temp and
sa_uses_index looked for data-names among the tokens; they walk the
resolved operands now (expr_names).  The snapshot diffs empty: every
program, asm and diagnostics, byte for byte.

A user-defined function met while scanning ahead made no call, and the
re-parse made it; the scan's operand now keeps the call (Opnd.uc), and
ucall_make makes it when the leaf is emitted -- the copies and result
made anew, as each parse made them.  That found a defect older than the
pass: **a function call inside another call's expression argument**,
twice(twice(a) + 1), gave 0.  Recording the inner call wrote it into
the g_ucall slot the outer call was being made from, so the outer call
passed the inner's arguments; emit_ucall now works from its own copy.
The same shape in a subscript (el(twice(twice(i) - 3) + 1)), a MOVE, a
DISPLAY, FUNCTION MAX's argument and an UNTIL condition all went wrong
with it.  The width scan's own pass had also laid out a result record
per call that nothing used (11 in the probe program); those are gone.
GnuCOBOL gives 0 for the nested call as well (docs/oracles.md).  Test
2002/userfnnest (.oracle-expected).

Step 2: **COMPUTE reads its expression once.**  It used to parse it
up to four times: a scan for the width, hx_expr for a register tree of
integers, dx_expr for one of decimals, then parse_expr for the stack's
code (and once more on the integer path's overflow branch).  Now the
scan's tree is all of them: hn_tree turns it into the register paths'
HNode tree, leaf by leaf through the path's own test (hx_leaf,
dx_leaf), and emit_expr writes the stack's code from it.  The function
arguments the register paths take (MOD, INTEGER and the rest) are
converted from their trees too, and O_EXPR no longer keeps a token
range.  A leaf keeps its first token, so the paths still refuse what
they refused before (at_operand there).  The snapshot differs in three
programs, 2002/userfn, userfndeep and userfnnest: every user function
call in a COMPUTE had a result record laid out for each of those
parses, which nothing used, each initialised at start-up or on entry;
they are gone,
and with them their entries in the recursive functions'
LOCAL-STORAGE tables.  Nothing else differs.

Step 3: **boolean expressions.**  parse_bexpr is a shunting yard that
emits as it applies each operator; it now also makes that operator's
node over its operands' (bool_apply), so the tree's postfix walk
(emit_bexpr) is the order the yard emitted in.  A boolean expression as
a condition operand (O_BEXPR) keeps the tree, not its tokens, and
bool_push walks it instead of re-parsing.  With that, no Opnd keeps a
token range.  The snapshot diffs empty; the boolean tests
(2002/boolexpr, boolreview, boolbit, bitarray2) are among it.

Step 4 begins: **ADD, SUBTRACT, MULTIPLY and DIVIDE as nodes.**  Each
is read whole into an Arith node -- operands, receivers with ROUNDED,
DIVIDE's REMAINDER item, and whether a SIZE ERROR phrase follows --
under g_noemit, so a user function among the operands is deferred.
Then the calls are made, in the order written (arith_calls), and the
statement's code follows from the node.  Before, the GIVING forms read
their second list twice (a scan to find GIVING, then again for real so
a call would be made), and DIVIDE read its REMAINDER item up to three
times: size_error_after_remainder and hx_remainder_ahead looked past
it, and emit_remainder parsed it again in the middle of emitting.  All
three are gone; emit_remainder takes the item.  The SIZE ERROR phrases'
own statements are still parsed where their code goes.

With the calls made from the node, ucall_make now uses the copies and
result record the scan made (clearing ftemp_scan) rather than making
new ones.  The scan's records were laid out in any case, so every
deferred call had left one set unused; with this, no call does.  The
snapshot differs in four programs, all with user functions -- 2002/
userfn, userfndeep, userfnnest and X-COBOL's fielded_to_linear (both
dialects) -- by removed records and their LOCAL-STORAGE table entries
only.  Test 2002/userfnarith: a function in every format of the four
verbs, REMAINDER and SIZE ERROR (twice(0) as a divisor); GnuCOBOL
agrees.

Next, **SEARCH**.  Its AT END and WHEN bodies go after the loop, which
holds only the tests, so each body was parsed twice: under g_noemit to
find where it ends, then again after the loop for its code (with two
"re-parse drifted" checks).  The assembly is held in memory until the
end (g_asm, for branch relaxation), and nothing reads it back before
then, so a stretch of it can move: a Block is the code a nested
statement list made, parsed once where it is written, cut out
(block_cut) and put where the statement wants it (block_put).  The
bodies' labels are now allocated before the loop's, so the snapshot
differs in the eleven programs with a SEARCH whose bodies use labels
(CCVS NC231A-NC237A, NC247A, IC207A; majesty gl034; free/search) --
and in nothing else once labels are renamed in order of appearance
(labnorm.py).

**An operand read twice.**  Wherever an expression may begin with an
operand -- a condition's operand, a user function's argument, an
intrinsic's argument -- the operand was read, an arithmetic operator
found after it, and the whole expression read again from its first
token.  expr_opnd_after continues the expression from the operand
already read (parse_primary takes it as the first leaf), so nothing is
read twice.  A user function beginning such an expression had been
called on the first reading and again by the expression: bump(0) + 0
counted two calls.

Writing the test for that found an older defect, from the start of user
functions (d3bc3958): **a condition that is a single relation made its
calls twice.**  parse_cond keeps a condition's calls on a copy of its
top node; cond_jump_false/true made them, then handed the same node to
emit_cond_value, which made them again.  IF twice(a) = 42 called twice
twice, and nothing noticed, twice having no side effect.  The jump
functions now pass on a copy without the calls.  The snapshot drops a
call in userfn (4), userfnnest (22), and in five of the X-COBOL
GnuCOBOL-mirror programs (today, main, main2, fielded_to_linear,
linear_to_fielded: isvaliddate in an IF), and changes nothing else.
Test 2002/userfnonce: a counting function in a condition, IF, an
argument, an intrinsic's argument, UNTIL and WHEN; GnuCOBOL counts the
same calls but passes 0 for the expression argument (.oracle-expected,
docs/oracles.md).

**References kept as token positions.**  A SCREEN SECTION entry's
USING/FROM/TO reference and a report's SOURCE and CODE identifier are
read in the DATA DIVISION before the items they name are laid out, so
they were kept as token positions and parsed where used -- a dynamic
screen slot's at every ACCEPT and DISPLAY, a SOURCE at every GENERATE.
Positioned DISPLAY/ACCEPT parsed its item and its LINE, POSITION and AT
identifiers, kept only their token positions, and parsed them again to
emit.  Now each is parsed once -- at the statement, or at first use --
and the Ref kept (SField.ref, line_r/col_r/at_r; RField.source;
Report.code_ref).  The snapshot diffs empty.

**EVALUATE's subject** is known to be an operand or the beginning of a
condition only after it is read; when a relation or class word
followed, it was read again as a condition, and a user function in it
was called by both readings (EVALUATE bump(0) = 1 counted two calls).
The subject is now read as a scan, and either read again as the
condition, whose calls the condition makes, or its own calls are made
there (ucall_make).  2002/userfnonce gained both forms; the snapshot
changes in no other program.

**MOVE as a node.**  MOVE already read its sender and receivers before
any code; only a user function in the sender was called while it was
read.  The sender is now read as a scan and its call made where the
code begins (ucall_make).  The snapshot then lost one call in 2002/
userfn: MOVE FUNCTION LENGTH(pad("xyz")) TO t.  LENGTH (and LENGTH OF,
BYTE-LENGTH, HIGHEST- and LOWEST-ALGEBRAIC) fold to a constant when
compiling, and the fold kept the length but dropped the argument's
pending call; the old MOVE had made the call only because it read the
sender with code on.  **The same loss was already in two earlier
steps**: COMPUTE emitting from its scanned tree (b4089eed) and the
arithmetic nodes (9a708fc4) skipped the call in COMPUTE b =
LENGTH(f(x)) + 0 and ADD LENGTH(f(x)) TO b.  The snapshot could not
see it -- no program in the corpus does that -- and a refactor checked
only by the snapshot is checked only where the corpus goes.  Now a
folded constant carries the argument's call (Opnd.uc), ucall_make makes
it, and the register paths refuse such a leaf (opnd_scanned) so the
stack makes it.  2002/userfnonce gained LENGTH in COMPUTE, MOVE and
ADD; GnuCOBOL counts the same calls.  The snapshot against the
previous commit diffs empty.

**IF as a node.**  parse_if reads the condition and both branches before
any code -- each branch's statements parsed once into a Block, NEXT
SENTENCE recorded -- and emit_if lays them out.  The branches' labels,
literals and descriptors are now allocated before the condition's, so
805 programs' assembly changed byte-wise and none in substance:
tests/asm-equiv.py (new; label families renamed in order of
appearance, code compared in sequence, data as a multiset) finds them
all the same code (the one exception, userfnonce, is the test's own
edit since the baseline).  bi2 compares the same way when bytes differ
and stays at same=227 diff=2.

**A branch that is one jump.**  With both branches parsed before the IF's
code, emit_if sees a branch that is a single jump -- GO TO p, NEXT
SENTENCE, EXIT PERFORM -- and makes it the condition's own branch to the
target (retarget: the condition jumps to a fresh label, and its lines
are rewritten to the target), instead of a branch around a jump; an
empty THEN (CONTINUE) becomes a branch round the ELSE.  The corpus had
5821 branch-over-jump shapes; 5301 are gone, about 7700 instructions
(3,202,015 -> 3,194,340 over the snapshot).  The 520 left are phrases
-- AT END GO TO, INVALID KEY GO TO -- for when the phrases are nodes.
This one changes code on purpose, so it was checked by running the
corpus: harness 769/0, majesty validate and functions, the papers, and
CCVS-85 run with the compiler before and after (identical reports: 348
programs, 8068 of 8175 pass, every one matching GnuCOBOL).  bi2,
byte-identity against the 2026-09-29 compiler, can no longer mean what
it meant: it reads same=58 diff=171, the differences being these
branches.

**The conditional phrases as nodes.**  [NOT] ON SIZE ERROR is read with
its statement, before any code (SizePh, for ADD, SUBTRACT, MULTIPLY,
DIVIDE and COMPUTE; the statement keeps its ROUNDED MODE and width
across the phrases' own statements), and every phrase pair -- SIZE
ERROR, AT END, INVALID KEY, AT END-OF-PAGE, ON EXCEPTION, ON OVERFLOW --
is laid out by one emitter, emit_phrases, from Blocks.  First with the
old layout (asm-equiv: the same code), then with two changes: a phrase
that is one jump (GO TO, NEXT SENTENCE) is the status test's own
branch, and an ON phrase with no NOT phrase no longer jumps past
nothing.  About 3000 instructions fewer over the snapshot.  (Counting
branch-over-jump shapes misleads here: a direct branch to a paragraph
more than 4000 bytes away is relaxed into exactly that shape.)

A change of code is checked by running.  CCVS-85 before and after:
identical.  tests/gen/run-self.sh (new) builds the compiler as of a git
revision and runs generated programs through both it and the current
one, requiring the same output bytes -- the compiler before a change as
the oracle for it, on as many programs as asked for, no container.
tests/gen/gen-flow.py (new) generates what this exercises: paragraphs
that trace themselves, forward GO TO, IF with GO TO / NEXT SENTENCE /
CONTINUE / ELSE GO TO, SIZE ERROR phrases, a file read with AT END GO
TO, CALL of a missing program with ON EXCEPTION.  100 programs: the
same.  Mutation-tested: a wrong branch sense in the status-1 test (AT
END, INVALID KEY) shows in 100 of 100 programs, in the nonzero test
(SIZE ERROR, EXCEPTION) in 62 of 100.  gen-flow is in Gate 7 against
GnuCOBOL too, where it found an oracle defect: for a CALL of a missing
program, GnuCOBOL 4.0-early-dev runs NOT ON EXCEPTION after an ON
EXCEPTION phrase that falls through (2023 14.9.4.4 rule 3h1 sends
control to the end of the CALL).  Test free/callexc (.oracle-expected);
run-gen.sh counts gen-flow's such lines apart.

**Calls once, first; EVALUATE as a node.**  A probe for EVALUATE -- a
counting function in a subject and in subscripts -- found four defects,
three of them older than the pass:

- **A compiler crash** (SIGSEGV; the pre-pass compiler too): MULTIPLY
  into an element whose subscript calls a user function, and COMPUTE
  with such an element on both sides.  The register paths emit from a
  tree in static storage (g_hn); emitting a leaf's subscript made the
  call, the call moved its argument with emit_move, and dx_move built
  its own tree in the same storage.  Now nothing rebuilds a tree being
  emitted (g_hn_busy; dx_move declines), and -- the real fix -- no call
  is made during emission at all.
- **ADD 1 TO el(f(x))** called f twice, once for each time the
  receiver's address was formed.
- **An EVALUATE subject** holding a call -- an expression, a condition,
  a subscript -- made it again for each WHEN tested.  2023 14.9.13.4
  rule 3: each subject is evaluated at the beginning.  GnuCOBOL makes
  them per WHEN too (docs/oracles.md).

The rule is item identification's (2023 14.6.4): the identifiers in a
statement are evaluated left to right as the first operation of its
execution, and function evaluation and subscript evaluation are steps
of that.  So ucall_make now makes every call in an operand once, in
place, in the order written -- its subscripts', its reference
modifier's, its expression's leaves', a function's arguments', its own
-- and the operand is then free of calls.  The node statements (the
arithmetic verbs, MOVE, COMPUTE) do that for their operands and their
receivers before any code; scan_expr does it for a statement that emits
as it reads (inside a condition the call is queued with the condition,
as before).  With the operands plain items, the register paths take
statements they refused: COMPUTE res = n * fact(m) is 64-bit arithmetic
in registers now, where it was the stack (the user-function tests grew
64 instructions and lost that many runtime calls).

EVALUATE reads its WHEN phrases whole (each one's objects with any code
reading them makes, its test, its statements as a Block), then lays
them out: a body that is one jump is its test's own branch; the last
body, and one that ends in a jump, need no jump to the end (IF's THEN
likewise).  About 500 instructions fewer over the corpus.  Checked by
running: tests/gen/gen-flow.py gained EVALUATE (items, expressions,
TRUE; THRU; stacked WHENs; OTHER) -- 40 programs against GnuCOBOL, 150
through the compiler before and after (run-self.sh), CCVS-85 before and
after.  Test 2002/userfnsub: the calls each statement made, the loop's
condition three times; .oracle-expected for the three EVALUATE lines.

**PERFORM as a node.**  parse_perform reads the phrases (UNTIL's
condition, VARYING's items, TIMES' count), then an inline body's
statements into a Block (parse_inline_body, with the EXIT PERFORM
labels), and only then emits the loop; emit_body places the block.  The
same layout as before: 203 programs' labels renumbered, all the same
code (asm-equiv).

**Loops tested at the bottom.**  A test-before loop was laid out as the
test, the body, and a jump back to the test; it is now one jump in, to
the test, and then the body and the test's own branch back -- PERFORM
UNTIL, VARYING (each level of AFTER), and TIMES (the count left kept in
r1 at the test, one less after each execution of the body).  WITH TEST
AFTER was already so.  The static size is the same (3,194,291
instructions over the snapshot, before and after); each iteration is
one instruction shorter: a program of five counted loops, 4.3 million
iterations, went from 220,123,749 instructions to 215,823,751.  What an
iteration mostly costs is the items, not the loop: a COMP item is
big-endian, so every access is a byte swap, a truncating rem and a
byte-wise store.

Checked by running.  gen-flow.py gained PERFORM: inline and of
paragraphs (THRU), TIMES, UNTIL, VARYING up and down, WITH TEST AFTER,
VARYING ... AFTER, nested inline loops, a GO TO out of an inline body.
60 programs agree with GnuCOBOL; 200 are the same through the compiler
before and after.  Mutation testing found a hole in the generator
first: a VARYING loop that skipped its first test passed all 60
programs, because every generated VARYING ran at least once.  With
loops whose condition holds at the start, that mutant shows in 12 of
60, and a TIMES loop that runs once too often in 60 of 60.  CCVS-85
before and after: identical.

**INSPECT as a node; the OVERFLOW phrases.**  Of the verbs left, STRING,
UNSTRING and CALL already read every operand before any code.  INSPECT
did not: it emitted cob_inspect_begin after reading the item, then read
each phrase and registered it, so a user function among the phrases'
operands was called between the runtime's begin and run -- and when
that function did an INSPECT of its own, the runtime lost the outer
statement: `INSPECT s TALLYING k FOR ALL f(x)` counted 4 dots for 6,
REPLACING and CONVERTING changed nothing.  Now the statement is read
whole as a scan (InspPh, InspRange), its calls are made (2023 14.6.4),
and the runtime's sequence is emitted in one piece.  The snapshot is
byte-identical but for the one program with a function in an INSPECT.
GnuCOBOL 4.0-early-dev has the fault itself, in INSPECT and in STRING
(0 tallied; the STRING stops), and refuses a user function as a
REPLACING or CONVERTING operand.  Tests 2002/userfninsp
(.oracle-expected) and 2002/userfninsp2 (no oracle).

STRING's and UNSTRING's [NOT] ON OVERFLOW are Blocks through
emit_phrases (parse_overflow_phrases; ON begins one only before
OVERFLOW, so an enclosing CALL's ON EXCEPTION is no longer taken for
it).  emit_phrases lays a two-valued status (SIZE ERROR, EXCEPTION,
OVERFLOW) out as one test with the phrases its arms, where it tested
again for the NOT phrase; the I-O status keeps its two tests (2, an
error already reported, runs neither).  About 260 instructions fewer
over the snapshot.  gen-flow.py gained STRING into a short item and
UNSTRING into too few receivers, with OVERFLOW phrases: 60 programs
agree with GnuCOBOL, 200 are the same through the compiler before and
after, two wrong-sense mutants show in 29 and 17 of 60; CCVS-85 before
and after identical.

Not every verb reads its operands before its code yet.  DISPLAY emits
each operand as it reads it, so DISPLAY "one " f(x) " two" shows "one "
before f runs -- and with UPON SYSERR, f's own DISPLAY goes to the error
stream.  An audit (a verb's first emit before its last operand parse)
also names WRITE, INITIALIZE, SET, ACCEPT, ALLOCATE and CALL's ADDRESS
OF as candidates.  Next: every statement as its calls, then its code.

**A statement is its calls, then its code; receivers at access.**
DISPLAY "one " F(X) " two" showed "one " before F ran, and under UPON
SYSERR F's own DISPLAY went to the error stream: DISPLAY writes each
operand as it reads it.  The fix is not DISPLAY's but every verb's.
ucall_emit cuts each call's code out of the stream as it is made
(stmt_call_cut), and parse_statement places the statement's calls
before its code.  A nested statement has its own list; a condition's
calls stay with the condition; an EVALUATE WHEN's objects keep theirs
(calls_scope_begin/end), evaluated when that WHEN is reached.

Reading the rules for that found that 4f4da3b5 had over-applied 14.6.4.
It says "unless otherwise specified", and for receiving items it is
specified otherwise: a MOVE's receiver is identified immediately before
the data is moved to it (14.9.25.4), an arithmetic statement's as each
is accessed (14.7.7 rule 4b), a DIVIDE's dividend as each is determined
and its REMAINDER after the quotient is stored (14.9.12.4), READ and
RETURN INTO's after the record is read and not at all when the read
fails (14.9.30.4, 14.9.34.4).  4f4da3b5 made receivers' calls first with the
rest; before the pass they were made as the address was formed -- the
right time, if twice.  Now those receivers are read as scans and their
calls made in place where the item is stored (recv_calls, held out of
the statement's list), once; a statement with such a receiver takes the
stack's stores (hx_ok, dx_ok, refs_hot, dec_add_ok decline).  Not yet
so, their calls made first: SET's receivers (immediately before each is
changed, 14.9.39.4), UNSTRING's, a PERFORM VARYING item.

Also: a subscript written FUNCTION f(x) was taken for a data-name
'function' (sub_is_expr); only the bare f(x) form was an expression.

The snapshot changes in five programs, all user-function tests; every
other program, CCVS-85 with them, is byte-identical.  Tests
2002/userfndisp (DISPLAY, the operands before and after a call, UPON
SYSERR, NO ADVANCING) and 2002/userfnrecv (MOVE, ADD, COMPUTE, DIVIDE
INTO, REMAINDER, READ INTO: each receiver's subscript a function of an
item the statement has just stored); GnuCOBOL agrees with both, line
for line.

**The rest of the receivers; EVALUATE's subject, once.**  Reading each
statement's own rules before finishing:

- SET: each receiver is identified immediately before it is changed
  (2023 14.9.39.4; X3.23-1985 SET general rule 3d -- db9926af's commit
  message says 5d, a slip).  Its receivers are read as
  scans and recv_calls makes their calls at each store: SET N EL(F(N))
  TO IX sets EL(3) when IX is 3.
- STRING and UNSTRING were listed above as not yet right, wrongly.
  X3.23-1985 (XVII-68, substantive changes 33 and 34) has their
  subscripting evaluated once, immediately before the statement -- a
  change from the 1974 standard, where it was undefined or done before
  each transfer -- and 2023 gives them no rule of their own, so 14.6.4.
  Calls first is what both say.
- PERFORM VARYING: the item's subscripting is evaluated each time it is
  set or augmented (X3.23-1985 XVII-64, substantive change 27; 2023
  14.9.28.4 rule 12).  A call made once cannot do that, so a user
  function in the item's subscript is refused, as BY's and an AFTER's
  FROM already are (bad/std2002-fn-varying-item).
- The exception-checking PERFORM stays as it is: no operands, and its
  code is its source order.

An EVALUATE subject that is an arithmetic expression or a numeric
function was evaluated again for each WHEN, twice for a THRU -- the last
case of "a subject is evaluated once" (2023 14.9.13.4 rule 3c;
X3.23-1985 EVALUATE general rule 1c).  With FUNCTION RANDOM it shows:
EVALUATE FUNCTION INTEGER(FUNCTION RANDOM * 6) + 1 with WHEN 1 ... WHEN
6 matched no face 184 times in 600, a new roll for each WHEN.  Now the
subject is evaluated at the beginning, its value taken off the numeric
stack into a compiler-made record (cob_nsave, in the wide form whichever
stack it came from) and pushed again for each comparison
(cob_npush_saved); a numeric function alone is treated as an expression
of one operand.  libcob gains those two functions and their wide
variants (the kit and the images want a refresh).  CCVS-85's IF module
tests its functions through EVALUATE ... WHEN x THRU y, so 38 programs'
code changed; the reports are identical.  GnuCOBOL rolls again for each
WHEN too: 148 of 600 on no face.  Test free/evalonce (.oracle-expected,
docs/oracles.md).  Left: an alphanumeric function subject.

**The two gaps left, closed.**

An alphanumeric, national or boolean function as an EVALUATE subject
was still evaluated for each WHEN.  MOVE's way of keeping a function's
value for several receivers (a copy of the bytes, fsaved) is not enough
here: a result of run-time length (TRIM) has its length in the runtime's
"result just evaluated" state, and a WHEN's own objects may evaluate
functions in between.  So the subject's result is kept with that length
in a compiler-made record (cob_fn_keep) and made the result just
evaluated again before each comparison (cob_fn_kept); emit_fn_value,
which every use of a function operand goes through, does that for an
operand with a kept result.  The functions being deterministic, the
output cannot show "once"; the code does (in 2002/evalfunc, cob_fn_trim
is called 7 times where it was 12), and the test has a WHEN whose object
is a shorter result before one that needs the subject's own length --
without the length restored it takes WHEN OTHER (mutation-tested).
GnuCOBOL agrees with the test.

With EC-DATA-INCOMPATIBLE checked, a receiver that is summed too (ADD a
TO b; SUBTRACT, MULTIPLY BY, DIVIDE INTO) had its content checked before
the arithmetic, which formed its address, which made a call in its
subscript -- first, not at the receiver's access.  Worse than the plan
said: ADD 2 TO N EL(F(N)) added to EL(1); DIVIDE 2 INTO N EL(F(N))
checked an element outside the table and raised the condition.  The
check for a receiver with a call still to make now waits for its access
(recv_access: the calls, then the check).  Test 2002/userfnrecvec (no
oracle): the four verbs as 2002/userfnrecv has them, checked, and a
receiver holding "1a3" still caught, there.

No program of the snapshot changes: none has such a subject or such a
receiver.

### 122. Performance, after the front-end pass (2026-10-01, in progress)

docs/performance.md has the measurement: where each kernel's and jerm's
time goes under the DBT -- generated code, libcob still translated, and
the native kernels behind the hooks.  bench/prof.sh and bench/prof.py
are the tool.  First change: the register path's division tells the
runtime how many fraction digits the receivers keep (cob_xdivn), and
ndiv_core makes no more; byte-identical output, karith -13%, kmove
-17%, kseq -10%.  Checked: 150 generated arithmetic programs the same
through the compiler before and after (GEN=arith run-self.sh), Gate 7,
CCVS-85 identical, all gates.

Second: alphanumeric moves whose lengths the compiler can count are
copies of the receiver's length -- a group to an item no longer than it;
a reference-modified part to or from an alphanumeric item or another
part, the sender as long as the receiver or longer.  kmove 484 ms ->
379; 6,453 of the snapshot's 39,814 runtime move calls gone.  Test
free/movefixed (each length relation, and the cases that stay with the
runtime; the same output with -fno-hot-arith; GnuCOBOL agrees).
cob_xdiv, which the compiler no longer calls, is removed: nothing is
released, so nothing is kept for old objects.  docs/plans/performance.md
is the staged plan: opportunistic work where the profile points,
operation-level hooks, values across statements, the generated code.

Third: checked 64-bit arithmetic.  A COMPUTE the pictures cannot prove
to fit in 64 bits -- a PIC 9(18) item times anything -- went to the wide
stack whatever its values.  It is now computed in 64 bits with each
operation's inputs tested (magnitude in bits: before a product, a
scaling, a sum that could pass 9*10^18), the wide stack's code behind
the tests, both storing the same value.  ksort 972 ms -> 475.  Checked
by tests/gen/gen-checked.py through the compiler before and after (150
programs; five mutants of the tests all caught, two only after the
generator gained a shape), tests/wide-differential.sh against GnuCOBOL,
CCVS-85 identical, free/checked64.  Found on the way: the wide store
cleared the sign of a negative value whose kept digits are zero, where
the narrow store and GnuCOBOL keep it (2023 14.9.25.4); the wide store
keeps it now, and DISPLAY shows a zoned zero as `+` (free/negzero).
run-self.sh counted two refusals as an agreement; fixed.

Fourth: measured the batch itself, not its stand-ins.  Majesty's
month-end run is 2.0 s under the DBT and csv2fw is 1.2 s of it; its
profile (bench/prof.py now counts calls, and -fprofile-lines attributes
instructions to source lines) is a wide COMPUTE, positions through the
stack, byte files and PERFORM -- not what the kernels showed.  First of
those: a subscript's or reference modification's integer expression is
computed in registers where it is used, and a DISPLAY subscript's
digits are read in line.  csv2fw 1.20 s -> 0.90, the same bytes out.
Checked by tests/gen/gen-pos.py through the compiler before and after
(100 programs; four mutants caught) and against GnuCOBOL (Gate 7's
gen/pos), CCVS-85 identical.  GnuCOBOL computes a position in an
unsigned BINARY operand's type: 8 - 9 + 2 wraps (tests/2002/refmodneg,
refmodnegp; docs/oracles.md).  The DIVIDE ... REMAINDER item is dropped
from the plan: on binary items it was in registers already.

Fifth, the rest of that profile.  NUMVAL of one character is a leaf of
the checked arithmetic (a digit in line, anything else the stack's):
the statement that was a quarter of csv2fw went from 8,000 instructions
to ninety.  READ and WRITE are small entries -- a fixed sequential READ
out of the runtime's block buffer, the common WRITE straight to fwrite
-- with the rest out of line (kseq 324 ms -> 237).  PERFORM keeps a
cell for each paragraph, the place of the frame waiting on its exit:
the push and the exit no longer search, and the end of a paragraph
nobody is performing costs a load, not a call; the frames' rules are
unchanged.  csv2fw 1.20 s -> 0.68, the batch 2.0 s -> 1.56.  Checks:
gen-checked.py's NUMVAL shapes, free/numvaldigit, free/seqblock,
tests/gen/gen-perf.py (PERFORM, GO TO, contained and recursive
programs; old against new, with the runtime of each), thirteen mutants
caught in all, CCVS-85 identical, all gates.  A mistake on the way: one
array of exit cells for the file, where paragraph numbers begin again
at each program -- CCVS-85's IC module found it, the generator had not,
and now would.

Sixth.  FUNCTION MAX and MIN are nodes of both register trees (a length
kept inside its item by MIN was the wide stack's); a part moved to a
part with a computed length is the alphanumeric move called directly,
its length checked as the descriptor's was; a position with a
subscripted operand is computed before the reference's offset begins;
and a numeric literal moved to an item is the bytes the store's own
kernel leaves, the kernel compiled into the compiler and run while
compiling.  csv2fw 1.20 s -> 0.42.  Checks: gen-pos.py and
gen-checked.py extended, tests/gen/gen-lit.py (every usage, every byte
printed), eleven mutants caught, CCVS-85 identical, all gates.  Found:
`&g_desc[sym_desc(s)]` -- the table's address read and a call that may
move the table, unsequenced; three places, one of them new.  The
compiler now passes a sanitizer build over every test, majesty's
sources and generated programs.

Seventh, outside cobol/: the C library.  fwrite was 91 instructions for
each byte csv2fw wrote; fwrite, fread and fputc in runtime/stdio.c are
now short entries in front of their general routines, a byte into a
buffered stream 28 instructions (runtime ISSUES-14).  csv2fw 0.42 s ->
0.37 -- 1.20 when the day began.  The test written for it found three
defects of the library, fixed with it (runtime ISSUES-15 to 17: output
after input that met end-of-file, lost; SEEK_CUR counted from the
read-ahead; ftell on an append stream from zero).  The platform's gates
ran with ours: regression 97, the cross-engine differential, SQLite,
Fortran, dBASE over majesty's reports, mdfix.

Eighth (2026-10-02).  PERFORM's push and the exit's pop are written out
by hand, with no frame: 43 and 30 instructions -> 17 and 16 (csv2fw 421
ms -> 391).  The LLVM backend learned tail calls (`llvm-backend/`;
csv2fw 351 -> 338 with nothing changed here), which is what lets an
entry with no call on its short path be C.  And READ and WRITE of a
fixed-length sequential record ask about the file once: four flag bytes
at the end of the file's block, set by the first record and cleared at
CLOSE; a one-byte record is then 28 instructions to read and 27 to
write, which were 55, and 52 and fwrite's 26 -- the WRITE storing into
the C library's stream buffer through three inlines of `<stdio.h>`.
The last I-O status is one word, zero for 00.  csv2fw 333 ms -> 290;
1.20 s when this began.  `docs/performance.md` has each.

Found on the way, by the tests written for it.  A full device was not
reported to a program whose records end where the stream's buffer does
-- every one-byte WRITE took 00 and 4,096 records were gone: the C
library's `fwrite` (runtime ISSUES-28), repaired.  And the runtime did
not build with the self-hosted compiler, the fallback of a machine
without LLVM (`cctool.sh`): libcob.c's file-scope asm of the same
morning (now `libcob/entries.s`, appended as the hook thunks are), and
in esql.c a local named for a typedef, which stage08 cc misread
(selfhost ISSUES-78), repaired in the compiler.
`tests/selfhost-libcob.sh` builds the runtime that way and runs the
suite's programs against it; it is one of the gates now.

Checks: free/seqbyte, 2002/seqbyteec, free/faultbyte, free/codesetrecs
(new), free/seqblock, free/faultwrite.  Forty-eight mutants of the new
paths, the status word, the header's inlines, the library's repair and
the compiler's extra word: thirty-seven caught -- six of them only
after 2002/seqbyteec was rewritten (a successful statement's status is
read only under EC-I-O-WARNING checking, which the first draft did not
turn on) and free/codesetrecs written (no test wrote a second record
through a CODE-SET).  The eleven that survive change nothing a program
can see: the file position and the last record length kept on the short
paths, which nothing reads for a file that is on them (six); the flags
never set, which is only slower (two); the read flags left set at
CLOSE, harmless because CLOSE also empties the buffer they guard (two);
and a truncation test that the count beside it implies (one).  CCVS-85
identical; all gates, and the platform's, since the C library and its
header changed.

