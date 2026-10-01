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
with `GEN=string`. For each INSPECT it also writes the expected line,
computed by `inspect85.py`, to `g<seed>.ref`. That file is the 85 INSPECT
rules (VI-96 to VI-99) written out independently of either compiler, and
it decides those lines: ours must equal it, even where the oracle agrees.

`gen-table.py SEED` covers table handling. It writes subscripted and
indexed references, with relative subscripts and indexing, and SET TO, UP
BY and DOWN BY. It writes SEARCH from a set starting index (a SEARCH
starts there, not at 1) and SEARCH ALL on an ascending unique key. It
also writes MOVEs from, to and between variable-length groups whose
OCCURS DEPENDING ON item is outside the group. Run it with `GEN=table`.

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

