# Differential testing on generated programs

`gen-arith.py SEED` writes a random COBOL 85 program. The program uses items
of random PICTURE and usage (DISPLAY, BINARY, PACKED-DECIMAL), and statements
drawn from ADD, SUBTRACT, MULTIPLY, DIVIDE (with and without REMAINDER), MOVE
and COMPUTE. About half use ROUNDED, and every one carries ON SIZE ERROR.
Each statement sets its operands from literals and DISPLAYs its result, so a
disagreement names one statement.

`run-gen.sh FIRST COUNT [STATEMENTS]` builds and runs the seeds here and
under the harness's GnuCOBOL images (`-std=cobol85`, one container for
the batch), then compares them line by line. A batch of 400 programs takes
seconds.

    tests/gen/run-gen.sh 1 400 70

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
- **GnuCOBOL: a remainder stored after the quotient's size error.**
  VI-81 rule 8a leaves both receivers unchanged. The case is recorded in
  docs/oracles.md, and run-gen.sh counts it apart as a known oracle defect.
