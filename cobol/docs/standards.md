# Standards first, dialects second

Written 2026-09-27. This is the direction for `./cobol` after v1, and
the reasoning behind it.

## Where this compiler sits

SLOW-32 is the layer. Execution engines sit below it: the DBT on
x86-64 and AArch64 for speed, and QEMU TCG for reach to every host
TCG targets. Languages sit above it, and COBOL is the first of them.
A `.s32x` built here runs on every engine below, and the differential
harnesses prove the engines agree on it.

That makes this compiler a different animal from the two free COBOLs
people already use. gcobol (the GCC front end) and GnuCOBOL both
compile to native code for the host at hand. They have speed, dialect
breadth and a community, and this compiler should not chase them on
any of the three. What it has by construction instead:

- **One artifact, run identically everywhere.** No recompiling per
  host, and no host C library or GMP underneath.
- **Longevity.** A compiled program is an archival object that runs
  wherever any engine does. For the language of forty-year-old
  ledgers, that may be the property that matters most.
- **A sandbox.** The program reaches only what the MMIO rings expose.
- **No dependencies at the target**, the property that made cobc370
  welcome on MVS 3.8j.

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

## Stage A — finish COBOL 85

X3.23-1985 plus the X3.23a-1989 intrinsic functions is the current
target. Per the README, CCVS-85 is at 348 of 348 programs compiling
and 8049 of 8160 tests passing. Closing that gap, or recording each
remaining failure as a ruling against the text, finishes the stage.

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
