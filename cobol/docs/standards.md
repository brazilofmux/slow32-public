# Standards first, dialects second

> **What this project is for.** Preservation first. The aim is a COBOL
> that no vendor can take away: old programs, compiled once, running
> identically on every engine SLOW-32 has, for as long as any of them
> runs. GnuCOBOL solved access to COBOL for everyone, and gcobol will
> carry it wherever GCC goes; this compiler is the owner's own, on a
> layer he designed, and it answers to no roadmap but his.
>
> It is tended slowly, one bounded piece at a time: a survey of a
> construct, a dialect behaviour locked behind a behavior point, a
> COBOL 2002 module with its own conformance tests. The standards
> below set the order of that work, not a schedule. COBOL 2002 and
> later have things people will want, and they will come, a piece at
> a time.
>
> A dialect is taken when real programs in it exist to be faithful to,
> as RM/COBOL was with the Open Systems suite. Without the code,
> matching a dialect is guesswork.
>
> This is a personal project, not an organisation. It is not taking
> feature requests; split keys and embedded SQL, for instance, arrive
> only if a program here needs them.

(Both examples have since arrived, and on those terms: embedded SQL
when majesty's PostgreSQL export moved onto SLOW-32 and the NIST SQL
Test Suite was there to gate it (docs/esql.md, 2026-09-29); split keys
with the Micro Focus programs that write them, where they turned out to
be the standard's own `SOURCE IS` first and the dialect's `=` spelling
second (conformance/files.md, 2026-10-01). The rule stands; the
examples were the next two programs' needs.)

Written 2026-09-27. This is the direction for `./cobol` after v1, and
the reasoning behind it.

## Where this compiler sits

SLOW-32 is the layer. Execution engines sit below it: the DBT on
x86-64 and AArch64 for speed, and QEMU TCG for reach to every host
TCG targets. Languages sit above it, and COBOL is one of them.
A `.s32x` built here runs on every engine below, and the differential
harnesses prove the engines agree on it.

That makes this compiler a different animal from the two free COBOLs
people already use. gcobol (the GCC front end) and GnuCOBOL both
compile to native code for the host at hand. They have dialect breadth
and a community, and this compiler should not chase them on either.
What it has by construction instead:

- **One artifact, run identically everywhere.** No recompiling per
  host, and no host C library or GMP underneath.
- **Longevity.** A compiled program is an archival object that runs
  wherever any engine does. For the language of forty-year-old
  ledgers, that may be the property that matters most.
- **A sandbox.** The program reaches only what the MMIO rings expose.
- **No dependencies at the target**, the property that made cobc370
  welcome on MVS 3.8j.

**Speed is not conceded.** Running on an emulated ISA does not make
this compiler the slow one. A native COBOL still pays for its runtime
model: GnuCOBOL routes MOVE, decimal arithmetic and editing through
generic `libcob` calls that inspect field types and pictures at run
time, with GMP under the decimals. This compiler knows both fields'
shapes when it lowers a statement and emits the specialized sequence,
and the DBT keeps SLOW-32 close to native. Compile-time specialization
is the lever, and every stage below keeps it: a feature that can only
be implemented through a generic run-time path is a cost to weigh, not
a free addition. Performance work is recorded in
[performance.md](performance.md).

The aim is to be the better answer to the questions those properties
answer, not to take users from anyone. Where this compiler finds
GnuCOBOL or gcobol disagreeing with the standard, a clean report with
a test case goes upstream. Three COBOLs aimed at different questions
are better for the language than any one of them winning.

## The order

1. **The standard, as text.** The standard is the authority and every
   implementation is an oracle ([oracles.md](oracles.md)). When they
   disagree, follow the text.
2. **Dialects afterward, with the test suite in hand.** Each dialect
   difference is taken deliberately, as a switch with its own tests,
   once the standard behaviour underneath it is pinned.

Dialects already taken, before this was written down: RM/COBOL's
positioned `DISPLAY`/`ACCEPT` for the Open Systems suite, and
`SCREEN SECTION`, both in [screen.md](screen.md). The second is less
of a bend than it looked: Micro Focus pioneered `SCREEN SECTION`, but
COBOL 2002 standardized it, so that work counts toward the standards
track.

Dialect breadth is where GnuCOBOL has paid for twenty years: every
behaviour switch multiplies a test matrix nobody can exercise whole.
This compiler stays narrow the way cobc370 did, and takes a dialect
when a real program needs it.

## One compiler, a `-std` switch

2002 goes in this directory, not a fork of it. The test is whether two
standards disagree or one extends the other. COBOL 74 and 85 disagree:
`PERFORM … AFTER` resets in a different order and a receiving ODO group
sizes differently, so the same source means different things, and
[borrowing.md](borrowing.md) is right that one parser for both is a
defect factory. COBOL 85 and 2002 mostly extend: `RECURSIVE`,
`LOCAL-STORAGE`, `NATIONAL`, `FUNCTION-ID` and exception handling are
new constructs, not new meanings for old ones. A 2002 fork would start
as a copy of the whole 85 compiler and need every later fix to the
shared core applied twice -- the drift the selfhost stages already
taught.

The focus a fork would give comes from a switch instead:

| switch | accepts | refuses |
|---|---|---|
| `-std=85` | X3.23-1985, the X3.23a-1989 intrinsics, and the implementor extensions already taken (free format, `SCREEN SECTION`, RM/COBOL positioned I/O, `USAGE POINTER`, the C-ABI `CALL`) | 2002 constructs, with a message naming the standard they need |
| `-std=2002` | all of the above, plus each Stage B module as it lands | modules not yet implemented, with a message, and OO |
| `-std=2014` (2026-10-07, queue item 20) | all of the above, plus 2014's additions as they land: the IEEE floating-point usages first | the same |
| `-std=2023` (2026-10-07, queue item 30) | all of the above, plus 2023's; the `>>FLAG-14` warnings | what 2023 removed from 2014 (conformance/edition-2023.md), and the same |
| `-dialect=mf`, `-dialect=gnucobol` (2026-10-01, 2026-10-04) | orthogonal to `-std`: Micro Focus's and GnuCOBOL's own forms (behavior-points.md classes D and G), refused without the switch naming it | -- |

- **`-std=85` is the default**, and majesty and CCVS-85 stay pinned to
  it. Nothing Stage B adds can change what an 85 program compiles to.
- **Refusal is part of the switch.** Under `-std=85` a 2002 construct
  is a diagnostic, never silently accepted, the way cobc370 refuses 85.
- **2002 gets its own suite**, `tests/2002/` beside `fixed/` and
  `free/`, and its own pass count.
- **Where 2002 changes 85 behaviour rather than extending it**, the
  difference goes behind the switch with a test on each side and is
  recorded here. If those ever pile up the way the 74/85 differences
  did, that is the signal to reconsider the fork. Nothing known yet
  suggests they will.
- 2014 and 2023 earned their values on 2026-10-07 (items 20 and 30), once the queue
  (docs/plans/standard-queue.md) made the current text the target: two
  more rows in the table, each accepting the one before, and each the
  place that edition's *removals* go behind the switch.
  **2014 did, 2026-10-07**: `-std=2014` exists, taking everything 2002
  does plus the 2014 additions as they land (docs/plans/standard-queue.md
  tier 3, item 20 first: the IEEE floating-point usages); `tests/2014/`
  is its suite. The 2014 points taken early as extensions (BP-E27 TRIM,
  BP-E29 ROUNDED MODE) are the language under it, no warning.
  **And 2023, 2026-10-07**: `-std=2023` takes everything 2014 does plus
  the 2023 additions as they land (standard-queue.md tier 4, item 30
  first: OPTIONS INITIALIZE, SYNCHRONIZED on a group), and refuses what
  2023 removed -- the class R behaviour points (behavior-points.md),
  the language through 2014 and silent there; `tests/2023/` is its
  suite, GnuCOBOL's -std=cobol2014 its nearest oracle.

**There is no `-std=74`, but 74-era programs are welcome.** The line is
between 74 *programs* and 74 *semantics*.

- **74 programs mostly compile as 85.** The 1985 text kept the old
  constructs as obsolete elements, still in the standard: `ALTER`,
  comment-entries (`AUTHOR.`, `REMARKS.` ...), `STOP literal`,
  `READ ... REVERSED`. This compiler implements them, CCVS-85 tests
  them, and that is how the Open Systems suite -- 228 programs of
  1978-83 RM/COBOL, a 74-era dialect -- came in (ISSUES.md 28).
- **RM/COBOL's extensions are a dialect, taken like any other**:
  positioned `DISPLAY`/`ACCEPT` ([screen.md](screen.md)), the device
  word in `ASSIGN`, `STOP RUN identifier`. If RM earns more, it can be a
  `-std` row of its own, layered on 85 the way `SCREEN SECTION` is.
- **What is not emulated is where 85 changed a 74 meaning**: the
  `PERFORM VARYING ... AFTER` reset order and the size of a receiving
  group containing ODO. Those are the only places a 74 program compiles
  cleanly here and silently computes something else. cobc370 has the
  receipts ([borrowing.md](borrowing.md)); full 74 conformance is its
  job, on the machine of that era.

**The hazard, and how it is closed.** Silent is the problem, not the
difference. `-warn-74` flags both shapes whose meaning changed, and the
obsolete elements a 74 program carries, so a 74-era corpus says out
loud where it needs updating. The points, their ids and the audit of
the Open Systems suite (no class M hits in 217 programs) are in
[behavior-points.md](behavior-points.md).

If `s32-cobc.c` (9,500 lines) outgrows navigation, the answer is to
split it into files within this one compiler, not to fork the
directory.

## Stage A — finish COBOL 85

X3.23-1985 plus the X3.23a-1989 intrinsic functions is the current
target. Per the README, CCVS-85 is at 348 of 348 programs compiling
and 8049 of 8160 tests passing. Closing that gap, or recording each
remaining failure as a ruling against the text, finishes the stage.

Surveyed 2026-09-27 (ISSUES-44): all 348 programs now match
GnuCOBOL's tally, 8068 of 8175 passing and none failing. No remaining
test is a failure. 16 are deleted by the suite itself and 91 are
marked for visual inspection, both the same as GnuCOBOL's. What is
left of the stage is those 91 read by a person against the text, and
the conformance gaps the suite does not test (ISSUES-43 is the first).

The 91 were read on 2026-09-27 (ISSUES-46): 24 print what GnuCOBOL
prints byte for byte; SQ101M and SQ207M state their own layout and
every claim holds, once print files became a line printer; SM106A
differs from GnuCOBOL only in where the file starts; NC114M inspects a
compiler listing, which this compiler does not produce. Two rulings
remain to be read against the text: where a print file's first line
falls, and the ADVANCING default. What is left of Stage A is those two
and ISSUES-43.

ISSUES-43 closed the same day: reserved words are refused as names,
with the five 74-era exceptions registered as behavior point BP-N1.
Stage A's remaining work is the two rulings above, each to be read
against the 1985 text.

Both were read the same day, once the text turned out to be public as
FIPS PUB 21-2 (docs/oracles.md). The ADVANCING default is AFTER 1 by
the text's own words (VII-54, WRITE rule 15), which the runtime already
does. The first-line placement is the implementor's, and the runtime's
choice matches how both compilers write LINAGE files (ISSUES-46).
**Stage A is complete:** CCVS-85 fully matches GnuCOBOL's tally with
none failing, every inspection test is accounted for, and no known
conformance gap is open.

Reopened in part 2026-09-28 ([refusals.md](refusals.md)). CCVS tests
what a compiler must accept, and barely what it must reject. A survey
of the refusals found four 85 features still refused (the CODE clause,
INITIALIZE and BY CONTENT of a reference-modified item, a REPORT
SECTION in a contained program) and one 85 syntax rule not enforced
(UNSTRING's reference-modified sending item). The rule-by-rule sweep in
refusals.md, "What follows", is what closes Stage A properly.

The four features and the rule were done the same day (ISSUES-94, -95),
with more the work turned up: INITIALIZE never set an item another
REDEFINES (the mask covered the redefined item's bytes), and an FD took
one report and INITIATE/TERMINATE one report-name. CCVS-85 is unchanged
by all of it; what the sweep may still find is unknown.

## Stage B — COBOL 2002, the practical half

The standard is treated as modules, not one block. These parts of
2002 are worth having whether or not a standard required them, and
they are what current COBOL code actually uses:

| feature | why |
|---|---|
| `RECURSIVE` programs, `LOCAL-STORAGE SECTION` | reentrancy; stack-local data |
| free-format source | already implemented as an implementor extension; the 2002 text makes it standard |
| user-defined functions (`FUNCTION-ID`, `REPOSITORY`) | see the note below |
| the 2002 intrinsic functions beyond the 1989 set | same module machinery as the 1989 ones ([functions.md](functions.md)) |
| exception handling: `RAISE`, `EC-` conditions, `USE` for exceptions | structured errors instead of status-code plumbing |
| `NATIONAL` data | Unicode; libutf (`scripts/build-libutf.sh`) already runs on SLOW-32 and is the natural runtime for it |
| `BOOLEAN` and bit data, `TYPEDEF`, `VALIDATE` | typed data the 85 language had to fake (`USAGE POINTER` is already accepted, as an implementor extension majesty needed) |
| `SCREEN SECTION` | done (see above) |

**Note on `FUNCTION-ID`.** [dialect.md](dialect.md) rules that this
compiler never learns `FUNCTION-ID` or `REPOSITORY`, because majesty's
corpus was rewritten to plain `CALL` instead. That ruling was about
what majesty needed, and it stands for majesty. Stage B revisits it
for the standard's sake, as a module with its own tests; majesty's
programs do not change.

Each feature lands the way every stage here has: refused with a
message until implemented, then tested against the text, with
GnuCOBOL as the oracle where it agrees with the text.

**Landed.** `-std=2002` exists; its tests are `tests/2002/`, run
against GnuCOBOL's `-std=cobol2002`, and under `-std=85` each module is
refused with a message naming the switch.

- **`RECURSIVE` and `LOCAL-STORAGE`** (2026-09-28, ISSUES-49). Each
  activation gets a fresh LOCAL-STORAGE set to its VALUEs, its own
  LINKAGE, PERFORM control and TIMES counters; a LOCAL-STORAGE item's
  address may be passed on and stays that activation's (2023 8.6.4).
  WORKING-STORAGE, file connectors, sort files, reports, index-names
  (and ALTER state) belong to the program and are shared by its
  activations, as static data is. A program contained in a recursive
  one is recursive. Calling an active program that is not RECURSIVE is
  EC-PROGRAM-RECURSIVE-CALL, fatal. How: an activation descriptor per
  program, read by `cob_act_enter`/`cob_act_leave` at entry and
  return; under `-std=85` none is emitted, and 85 output is unchanged.
- **User-defined functions** (2026-09-28, ISSUES-50). `FUNCTION-ID`,
  `REPOSITORY`, invocation with or without the word FUNCTION, BY
  REFERENCE and BY CONTENT arguments, and an external repository of
  signature files so separately compiled functions keep call sites
  specialized. Proven on majesty's original 2002 date family
  (`tests/majesty-functions.sh`). docs/functions.md has the design and
  what is still refused.
- **Free-form reference format**, checked against the text (2026-09-28,
  ISSUES-51). Free format was already here as an implementor extension;
  what 2002 adds is now implemented under `-std=2002`: `>>SOURCE FORMAT
  IS FIXED | FREE` switching mid-text, and literal continuation with the
  floating indicator `"-` or `'-` in either format, comment lines allowed
  between the parts. The floating comment `*>` stays accepted in both
  standards, as majesty needs. Other compiler directives (`>>DEFINE`,
  `>>IF`, `>>D` ...) are refused by name.
- **The 2002 intrinsic functions** that need no other module (2026-09-28,
  ISSUES-52): the numeric, date-window, BYTE-LENGTH, NUMVAL-F and
  TEST-NUMVAL families; the rest refused naming the module or edition
  they need (docs/oracles.md, "Intrinsic functions, COBOL 2002").
- **Exception handling, first part** (2026-09-28, ISSUES-53): `>>TURN`
  (by exception-name, group or EC-ALL, ON [WITH LOCATION] or OFF, from
  its point in the source), `RAISE EXCEPTION`, `USE AFTER EXCEPTION
  CONDITION` declaratives chosen most specific first, fatal conditions
  ending the run after their declarative, EXCEPTION-STATUS and
  EXCEPTION-STATEMENT, `SET LAST EXCEPTION TO OFF`. Everything is decided
  at compile time, and a program that never turns checking on compiles
  as before. EC-SIZE from the arithmetic statements followed (ISSUES-55),
  then EC-BOUND-SUBSCRIPT and EC-BOUND-REF-MOD (ISSUES-56), then EC-I-O
  from the I-O status (ISSUES-58), then EC-PROGRAM-NOT-FOUND (ISSUES-59).
  and EC-PROGRAM-RECURSIVE-CALL at the caller (ISSUES-60), and
  EC-BOUND-ODO (ISSUES-61), and EXCEPTION-LOCATION and EXCEPTION-FILE
  with their national forms (ISSUES-65), and TURN for one file
  (ISSUES-87), and 2023's exception-checking PERFORM (ISSUES-89). Still
  to come: the rest of Table 13's conditions. No oracle: GnuCOBOL 4
  does not implement exception declaratives.
- **NATIONAL** (2026-09-28, ISSUES-62 to -75). docs/national.md.
  - Part one: PICTURE N and national literals, stored UTF-16 big-endian,
    alphanumeric read as UTF-8; VALUE, MOVE, comparison, DISPLAY, LENGTH
    and INITIALIZE.
  - Part two: NATIONAL-OF, DISPLAY-OF and CHAR-NATIONAL, with function
    results whose length is known only at run time; EXCEPTION-LOCATION
    and -FILE; UPPER-CASE and LOWER-CASE on national and UTF-8 text.
  - Part three: reference modification, INSPECT, STRING, UNSTRING,
    ACCEPT, national groups, numeric and numeric-edited USAGE NATIONAL,
    national-edited pictures, and national records in files (line
    sequential as UTF-8).
  - National fields in Report Writer and SCREEN SECTION (ISSUES-92),
    laid out by display width on a Unicode-aware term service.
- **BOOLEAN, part one** (2026-09-28, ISSUES-76): PICTURE 1 in USAGE
  DISPLAY and NATIONAL, B and BX literals, VALUE, MOVE, comparison, the
  boolean condition and class test, INITIALIZE, BOOLEAN-OF-INTEGER and
  INTEGER-OF-BOOLEAN. Part two (ISSUES-77): boolean expressions in
  COMPUTE and conditions. Part three (ISSUES-78): USAGE BIT, packed at
  bit positions, and GROUP-USAGE BIT. docs/boolean.md.
- **TYPEDEF and TYPE** (2026-09-28, ISSUES-79): type declarations,
  expanded over the tokens as the text defines them; STRONG types
  (ISSUES-80) with their MOVE and comparison rules. docs/typedef.md.
- **EXIT PERFORM [CYCLE], EXIT PARAGRAPH, EXIT SECTION, PERFORM UNTIL
  EXIT** (2026-09-28, ISSUES-90): 2023 14.9.14 formats 3-4 and 14.9.28
  general rule 11.
- **VALIDATE: not built, by ruling** (2026-09-28). 2023 marks the
  VALIDATE facility obsolete (D.22), and its list of obsolete elements
  (F.2 item 5) records that no COBOL provider has implemented it and
  that neither users nor implementors have asked for it, its future to
  be weighed at the next revision. It has been optional since 2014
  (2014 A.4.13). It stays refused by name here until a program needs
  it. (Corrected 2026-10-06: this entry placed the remark in Annex E
  and quoted it; it is F.2's, paraphrased here.)

## Deferred — object orientation

COBOL 2002's classes, `INVOKE`, interfaces, `FACTORY`, method
overloading and object references. It is in the standard, and it is
still required: 2014 and 2023 make only multiple inheritance and
parametric polymorphism optional (2023 A.4.10). So the deferral is the
owner's, not the standard's. The reason is use: the industry never
took it up for business code -- where it lives it is glue to Java and
.NET, which a program on SLOW-32 has no use for -- and later revisions
did not push it further. The objection is not to object orientation
(C++ is fine) but to OO COBOL specifically: verbose on top of verbose.

Ruled 2026-10-06: deferred, not excluded. The owner means to be able to
say the standard is implemented as far as it can be, and object
orientation is part of it; it comes last, after everything else in
docs/plans/standard-queue.md, as its own module with its own design
note.

It waits until someone brings a program that needs it. By then Stages
A and B give it a suite to land against. It also cannot compromise the
layer: object references, dispatch and whatever memory management it
needs all live in the COBOL runtime above SLOW-32, and none of it
touches the ISA.

## Later revisions

Surveyed 2026-09-28 against ISO/IEC 1989:2014 and 1989:2023, both held
(licensed copies outside the tree; oracles.md). 2014's Annex E lists
its changes from 2002, 2023's its changes from 2014. For Stage B:

- **Every Stage B module is still in 2023.** `RECURSIVE`,
  `LOCAL-STORAGE`, `FUNCTION-ID` and `REPOSITORY`, `TYPEDEF`, `BOOLEAN`
  and `USAGE BIT`, `NATIONAL`, and `RAISE` with the `EC-` conditions
  all appear in 2002, 2014 and 2023 alike. Stage B targets the current
  text with no module dropped.
- **Two modules changed shape.** 2023 removed `EXIT FUNCTION` and
  `EXIT METHOD` (E.2 item 1, page 1172), so a user-defined function
  ends with `GOBACK`, which 2023 also lets carry the STOP status phrase.
  2023 added an exception-checking form of `PERFORM` (E.2 item 19), an
  inline alternative to `USE` for exceptions. Stage B implements the
  2023 forms, not the 2002 ones.
- **What is optional now.** 2014 made optional what 2002 required
  (its E.2 item 23; the list is its A.4): screen handling, file
  sharing and record locking, Report Writer, the `RESUME` statement,
  `VALIDATE`, dynamic-capacity tables, locale support and others -- of
  object orientation only multiple inheritance and parametric
  polymorphism. 2023's list (A.4) adds Commit and Rollback and drops
  ARITHMETIC IS STANDARD, which 2023 removed. An implementation may
  omit these and still conform. The owner's ruling (2026-10-06) is to
  implement everything standard regardless; the order is
  docs/plans/standard-queue.md. (Corrected 2026-10-06: this said 2023
  added VALIDATE, and that the standard made object orientation
  optional.)
- **What 2023 took out that 85 programs use.** Continuation of a word
  in fixed form, `CALL ... ON OVERFLOW`, and `CLOSE ... WITH LOCK` with
  status 38 are removed (E.2 item 1). So is a figurative constant moved
  to a numeric item, except `ALL` with a literal of digits to an
  *integer* item, which 2023 keeps as obsolete (Annex F): the case behind BP-O9 (ISSUES-47),
  still defined, now only for integers. None of this touches `-std=85`,
  and all of it is on the watch list in
  [behavior-points.md](behavior-points.md) for the day a 2023 switch
  needs points of its own.

The order of Stage B is unchanged: `RECURSIVE` and `LOCAL-STORAGE`
first, the module the rest (user-defined functions especially) stand on.

## What this does not change

- The ISA. When COBOL needs more, the fix goes in this compiler or its
  runtime. The libutf port took four compiler fixes and no ISA change.
- cobc370 stays COBOL 74 ([borrowing.md](borrowing.md)). The two
  compilers share test knowledge, not a parser.
