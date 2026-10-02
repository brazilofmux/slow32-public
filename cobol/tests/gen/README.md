# Differential testing on generated programs

`gen-arith.py SEED` writes a random COBOL 85 program. The program uses items
of random PICTURE and usage (DISPLAY, BINARY, PACKED-DECIMAL), and statements
drawn from ADD, SUBTRACT, MULTIPLY, DIVIDE (with and without REMAINDER), MOVE
and COMPUTE. About half use ROUNDED, and every one carries ON SIZE ERROR.
Each statement sets its operands from literals and DISPLAYs its result, so a
disagreement names one statement.

`gen-edit.py SEED` writes MOVEs into edited items. Numeric-edited
pictures are built from the structure the 85 text gives them, so each
is valid by construction: fixed signs and currency, Z and `*`
suppression, floating `+ - $`, simple insertion, CR and DB, and BLANK
WHEN ZERO. Alphanumeric-edited pictures use X with `B 0 /`. Each label
carries its picture. Run it with `GEN=edit`.

`gen-cond.py SEED` writes conditions, each printing T or F. It covers
relation conditions across usages and scales, alphanumeric operands of
unequal length, figurative constants, and integer DISPLAY items against
alphanumeric ones. It also covers class and sign conditions, and combined
conditions with AND, OR and NOT. Abbreviated combined relations include
the NOT-before-an-operator forms. Run it with `GEN=cond`.

`gen-string.py SEED` writes STRING (several sources, DELIMITED BY, a
POINTER sometimes out of range, OVERFLOW), UNSTRING (ALL and OR
delimiters, DELIMITER IN, COUNT IN, POINTER, TALLYING, OVERFLOW) and
INSPECT (TALLYING, REPLACING, both, CONVERTING, BEFORE and AFTER). Run it
with `GEN=string`. For each STRING, UNSTRING and INSPECT it also writes
the expected lines to `g<seed>.ref`, computed by `string85.py` and
`inspect85.py`. Those files are the 85 rules written out independently of
either compiler, and they decide those lines: ours must equal them, even
where the oracle agrees.

`gen-table.py SEED` covers table handling. It writes subscripted and
indexed references, with relative subscripts and indexing, and SET TO, UP
BY and DOWN BY. It writes SEARCH from a set starting index (a SEARCH
starts there, not at 1) and SEARCH ALL on an ascending unique key. It
also writes MOVEs from, to and between variable-length groups whose
OCCURS DEPENDING ON item is outside the group. Run it with `GEN=table`.

`gen-pos.py SEED` covers computed positions: subscripts and reference
modification whose values are expressions, over operands of sixteen
usages and pictures, with every position in range by construction (the
position is chosen first, the operands' values worked back from it).
Run it with `GEN=pos`; both sides compile it as COBOL 2002 (an
expression subscript is not in the 85 text). Under `run-gen.sh` it keeps
every intermediate result at zero or above: GnuCOBOL computes a position
in an unsigned BINARY operand's type (`docs/oracles.md`, 2002/refmodneg).

`gen-perf.py SEED` writes programs of out-of-line PERFORMs behaving as
old programs do: GO TO in and out of what is being performed, exits
fallen through, contained programs and a second program in the file
that leave from the middle of a PERFORM, a program calling itself. The
standard leaves most of that undefined and the runtime's rulings are
its own, so GnuCOBOL is not asked: run it with `GEN=perf` under
`run-self.sh`.

`gen-lit.py SEED` moves numeric literals -- fitting, too long, with a
fraction the item cuts, signed into unsigned, zero -- to numeric items of
every usage and picture, and prints each item's value and every byte it
occupies. The compiler works such a store out when compiling; the bytes
must be the runtime's. Run it with `GEN=lit` under `run-self.sh` (a dump
of binary items is this machine's, so GnuCOBOL is not asked).

`gen-checked.py SEED`, `gen-pos.py`, `gen-perf.py` and `gen-lit.py` are run through
`run-self.sh REV FIRST COUNT`: the compiler and runtime as of a git
revision and the ones in the tree, every program's output the same
bytes (a run is capped, so a program a broken runtime sends round for
ever differs instead of hanging the batch). That is the check for a
change that alters generated code or the runtime on purpose, where what
came before the change is the oracle (`gen-checked.py`: statements
whose stores the standard leaves undefined, so GnuCOBOL cannot judge
them).

## References: the text, executable

Every generator also writes each statement's expected line to
`g<seed>.ref`. The line is computed from the
85 rules written out in Python, independently of either compiler:

- `arith85.py`: storing a result (alignment, ROUNDED, size error, an
  unsigned receiver), MOVE, and DIVIDE ... REMAINDER (VI-80, VI-81).
- `edit85.py`: editing rules 4 to 8 (VI-33 to VI-35), the sign table and
  BLANK WHEN ZERO.
- `inspect85.py`: INSPECT (VI-96 to VI-99).
- `string85.py`: STRING (VI-131 to VI-133) and UNSTRING (VI-137 to VI-139):
  the pointer, overflow, several delimiters in the order written, ALL,
  DELIMITER IN, COUNT IN and TALLYING. A STRING with a POINTER of 0
  breaks rule 5 and gets no reference line.
- `cond85.py`: relation, class and sign conditions, combined conditions,
  and abbreviated combined relations parsed as the text expands them
  (VI-54 to VI-61). The native collating sequence, the implementor's, is
  ASCII here.
- `table85.py`: SEARCH from a set index, the index past the table after
  AT END (VI-124, rule 2); SEARCH ALL (rule 4); and MOVEs from, into and
  between variable-length groups at their current length (VI-28,
  OCCURS rule 3a).

run-gen.sh judges every line a reference covers by the reference. Ours
must equal it, even where the oracle agrees with us, and an oracle that
differs from it is counted apart. GnuCOBOL becomes a cross-check rather
than the judge. A reference is only as good as its reading. Three
readings were corrected against the rule's exact wording after meeting
real output: LEADING's start, the floating string's right limit, and the
remainder's quotient. The last went the other way: the reference held,
both compilers departed from the 85 text, and the user ruled to follow
each edition as its text says.

`run-gen.sh FIRST COUNT [STATEMENTS]` builds and runs the seeds here and
under the harness's GnuCOBOL images (`-std=cobol85`, one container for
the batch), then compares them line by line. A batch of 400 programs takes
seconds.

    tests/gen/run-gen.sh 1 400 70
    GEN=edit tests/gen/run-gen.sh 1 400 60
    GEN=cond tests/gen/run-gen.sh 1 400 60
    GEN=string tests/gen/run-gen.sh 1 400 40
    GEN=table tests/gen/run-gen.sh 1 400 50

The harness runs seeds 1 to 40 of each generator as Gate 7
(`gen/arith`, `gen/edit`, `gen/cond`, `gen/string`, `gen/table`), about
11,000 checks a run. A mutation check showed it fails with the old `0` insertion bug back
in place (18 of 40 edit programs disagree). Without the oracle's image the
summary says the gate did not run.

The programs stay where X3.23-1985 defines the result exactly, so a
disagreement is a finding, not two valid choices:

- COMPUTE uses `+ - *` only. The precision of intermediate results belongs
  to the implementor, so a division inside an expression is left out.
- Division is the DIVIDE statement, whose truncation, ROUNDED and REMAINDER
  are defined (VI-80, VI-81).
- ON SIZE ERROR makes an oversized result, or a zero divisor, defined too.

When the two disagree, the text decides (docs/oracles.md).

## What it has found (2026-09-30)

- **s32-cobc: a REMAINDER computed with too few digits.** The product of
  quotient and divisor overflowed 64 bits although every item fit in 18
  digits. Test: free/divremse.
- **s32-cobc: MULTIPLY shed fraction digits.** The narrow stack drops the
  operands' fraction digits to fit 64 bits. That is acceptable for an
  expression's intermediate, but not for a MULTIPLY's result, which the
  standard defines exactly. Test: free/mulwide.
- **s32-cobc: COMPUTE lost its eighth significant digit.** This came from
  the same shedding inside a product of three items. Test:
  free/computewide.
- **s32-cobc: ROUNDED into an 18-digit receiver truncated.** Rounding needs
  a 19th digit of the quotient. Test: free/divround18.
- **The oracle: a remainder stored after the quotient's size error.**
  VI-81 rule 8a leaves both receivers unchanged. The case is recorded in
  docs/oracles.md, and run-gen.sh counts it apart as a known oracle defect.
- **s32-cobc: the `0` insertion character was never replaced** inside a
  zero-suppression or floating string. `$0$$.99` holding .42 gave
  ` 0$.42` for `  $.42`. An insertion character before any such string
  took the fill, so `/999` gave ` 012`. Both were editing rules 7 and 8.
  Test: free/editins.
- **The oracle reads rules 7 and 8 differently for `/`, `,` and B:** it
  keeps them in the suppressed part, and prints a B in a check-protected
  picture as `*`. Recorded in docs/oracles.md. run-gen.sh counts exactly
  those shapes apart, and a mutation check showed it still reports this
  compiler's old `0` behaviour.
- **s32-cobc and the oracle: STRING overflowed with nothing to move.**
  A POINTER past the receiver is the overflow "before each move of a
  character" (VI-133, rule 9), so a STRING whose sources give nothing
  to move ends without it. Both compilers tested the pointer on entry;
  MS COBOL 5.0 does too. Found by string85.py on 12 of 400 programs.
  Test: free/strovf.
- **s32-cobc: a negative value truncated to zero kept its sign.**
  −32745520.019 into `$9+` gave `$0-`. The value edited is the value
  after truncation (rule 7), and zero is "positive or zero" in the sign
  table, so the result is `$0+`. Test: free/editins.

After the fixes, 800 edit programs (about 48,000 MOVEs) agree apart from
the oracle's insertion readings above.
- **s32-cobc: an abbreviated relation dropped the NOT of the operator it
  implied.** In `a > b AND NOT < c OR d`, d takes `NOT <`: X3.23-1985
  VI-61 gives the expansion `... OR (a NOT < d)`. An abbreviation that
  stated its own operator was not recorded as the last one, and a NOT
  before an operator was read as the logical NOT. Test: free/abbrnot
  checks the text's five examples against their stated expansions over
  every a, b, c, d in 1..3.
- **The oracle reads a negative literal with more integer digits than the
  subject as unsigned.** `n00 >= -316940` is false there for an
  `S9(4)V9(3)` item holding 9884.108, but the comparison is algebraic,
  whatever the literal's length (VI-55). gen-cond.py labels such a
  condition with its computed truth. run-gen.sh counts the oracle's answer
  apart only when ours equals that truth. Test: free/negcmp.
- **The oracle refuses `... OR NOT b NOT < c`,** a negated relation whose
  operator has its own NOT, so the generator keeps such relations
  positive.

400 condition programs, about 32,000 conditions, agree after these.
- **The oracle applies several INSPECT phrases one after another over the
  whole item.** The 85 comparison cycle tries them position by position,
  in the order written, with the first match winning (VI-96, rule 6). Ours
  follows the rule. Test: free/inspord. Generated INSPECTs are checked
  against inspect85.py.
- **The oracle refuses an ALL or LEADING group after a CHARACTERS phrase
  in one TALLYING or REPLACING list,** so the generator writes CHARACTERS
  last.
- **s32-cobc: every receiving group over an OCCURS DEPENDING ON table took
  its maximum length.** X3.23-1985 VI-27, OCCURS rule 3, uses the maximum
  only when the DEPENDING ON item is inside the group (3b). With the item
  outside, only the part its value gives is used, receiving as sending
  (3a). A MOVE into a record built at a shorter count overwrote the rest.
  The BP-M2 behavior point now names the 3b case it is about. Test:
  free/odorecv. free/odomove and docs/oracles.md had recorded the
  maximum as the 85 rule for this case, and listed the oracle's current
  length as a divergence. That was a misreading: the text's list of
  changes since 1974 (XVII-54, change 8) names only a group that contains
  its DEPENDING ON item. It is corrected, and the oracle was right.

420 table programs, about 21,000 statements, agree. Subscripts, indexes,
SEARCH and SEARCH ALL found nothing else.
- **COBOL 85's DIVIDE ... REMAINDER uses the quotient as stored**, its
  magnitude in an unsigned item (VI-81, rule 6). 2002 and 2023 use a
  signed subsidiary quotient (14.9.12, general rules 6c and 7). Both
  compilers took the signed one under 85 too. The user's ruling: each
  edition as its text says, so s32-cobc now follows 85 under `-std=85`.
  Tests: free/divremu and 2002/divremu. The case was found by arith85.py.

`gen-loop.py SEED [LOOPS]` writes in-line loops over binary items whose
bodies change those items in every way that is not a store to the item
by name -- redefinitions, the group, a table over it, a performed
paragraph, READ INTO, a FILE STATUS, the runtime's stores -- for the
registers a loop's items are kept in (`src/cobc/loopreg.h`).  It runs
through `run-flag.sh FLAG FIRST COUNT`: the same compiler twice, as it
is and with FLAG (`-fno-loop-reg`), the two programs printing the same
bytes.  The harness runs 60 of them (gen/loop); no container is needed.

