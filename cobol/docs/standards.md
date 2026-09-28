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
- If 2014 or 2023 earn a value later, they are more rows in the table.

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

## Deferred — object orientation

COBOL 2002's classes, `INVOKE`, interfaces, `FACTORY`, method
overloading and object references. It is in the standard; almost no
production code uses it; later revisions did not push it further.
The objection is not to object orientation (C++ is fine) but to OO
COBOL specifically: verbose on top of verbose.

It waits until someone brings a program that needs it. By then Stages
A and B give it a suite to land against. It also cannot compromise the
layer: object references, dispatch and whatever memory management it
needs all live in the COBOL runtime above SLOW-32, and none of it
touches the ISA.

## Later revisions

ISO/IEC 1989:2014 and 1989:2023 revised the language again. Which
2002 features they made optional, obsolete or changed has not been
checked here; that survey happens before Stage B picks its modules,
so the stage targets the current text rather than a superseded one.

## What this does not change

- The ISA. When COBOL needs more, the fix goes in this compiler or its
  runtime. The libutf port took four compiler fixes and no ISA change.
- cobc370 stays COBOL 74 ([borrowing.md](borrowing.md)). The two
  compilers share test knowledge, not a parser.
