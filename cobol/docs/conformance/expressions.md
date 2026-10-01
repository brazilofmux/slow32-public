# Arithmetic expressions: 8.8.1 (and concatenation, 8.8.3)

Swept 2026-09-30. X3.23-1985: 6.2 (VI-51..VI-53). 2023: 8.8.1, 8.8.3.
CCVS-85 exercises expressions throughout its NC arithmetic programs, but
never chains exponentiation and never raises exponentiation's size
errors. The sweep went after those, and after the one rule the text
leaves to the implementor: how an expression is evaluated.

## 8.8.1.2 rules that hold in every mode

| rule | paraphrase | disposition |
|---|---|---|
| 1 | parentheses first, innermost first | **test**: free/arithexpr, fixed/compute, CCVS NC |
| 2 | unary, then exponentiation, then multiplication and division, then addition and subtraction | **test**: free/arithexpr (`- 2 ** 2` is 4, the unary minus first; `2 * 3 ** 2` is 18) |
| 3 | within a level, left to right | **test**: free/arithexpr: `8 / 4 / 2` is 1, `10 - 4 - 3` is 3, and **`2 ** 3 ** 2` is 64**. Exponentiation had been taken right to left (512), as GnuCOBOL takes it; fixed by this sweep. Micro Focus's reference states the same rule; no corpus chains `**` |
| 4, Table 3 | the permitted pairs of symbols | **refused**: two operands in a row, a leading binary operator, a trailing operator and unbalanced parentheses all stop the compile. A binary operator followed by a unary one (`a * - b`, `a ** + 2`) is permitted, and taken |
| 5 | how an expression begins and ends; parentheses paired; a leading unary after an identifier needs parentheses | **refused** as for rule 4; NOTE 2's subscript case is the subscript parser's ("expected a subscript") |
| 6a | zero raised to a power not above zero: EC-SIZE-EXPONENTIATION and the size error | **test**: free/arithexpr (`0 ** 0`, `0 ** -1`), 2002/ecsizeexp. Before this sweep `0 ** 0` gave 1 (the integer exponent's loop never looked at the base), and the size errors that were raised named EC-SIZE-OVERFLOW; the runtime now has a fourth size-error kind for them. GnuCOBOL gives 0 for `0 ** -1` (docs/oracles.md) |
| 6b | of two real results, the positive one | **test**: free/arithexpr (`4 ** 0.5` is 2) |
| 6c | a negative base needs an integer exponent, else EC-SIZE-EXPONENTIATION | **test**: free/arithexpr (`-8 ** 0.5`), 2002/ecsizeexp (`-8 ** (1 / 3)`: the exponent is not an integer, though a real cube root exists) |
| 7 | expressions are free of the composite-of-operands limit | **test**: fixed/compute; arithmetic.md has the statements' limit |

## 8.8.1.3 native arithmetic: the implementor's techniques

The text asks the implementor to state them (X3.23-1985 6.2.3 (6) asks
the same). This section states them. It is a **ruling**. Every path
below gives the decimal stack's answer where the stack can compute it.
The harness proves that on every run by compiling each program a second
time with `-fno-hot-arith` and running all of CCVS both ways
(docs/performance.md, the both-paths gate).

- **The decimal stack, the reference.** Each value is a 64-bit integer
  with a scale.
  - **Addition and subtraction** scale the operand with fewer decimals
    up while it fits. When it does not fit, the larger scale sheds
    fraction digits instead.
  - **Multiplication** sheds fraction digits from the more precise
    operand until the product fits in 64 bits with a scale of at most
    18. A product that still does not fit is a size error.
  - **Division** is exact long division. It runs to the operands'
    larger scale plus six guard digits, at least nine and at most
    eighteen, or until the quotient holds seventeen digits, and it
    truncates. The receiver's ROUNDED rounds that quotient.
  - **Exponentiation** by a non-negative integer exponent is repeated
    multiplication, exact within the stack's limits. A fractional or
    negative exponent is computed in double.
- **31 digits** (-std=2002, docs/wide.md): an operand, a receiver or a
  composite past 18 digits puts the statement on the wide stack. Its
  values are 128-bit magnitudes, and it follows the same rules to 38
  digits.
- **Floating point** (docs/usage.md): a float operand or receiver puts
  the whole statement in double. Each decimal operand is converted as
  it is pushed.
- **Integers in registers** (s32-cobc `hx_*`): binary and short DISPLAY
  integers are computed in a word in one of three modes.
  - When every intermediate provably fits, no check is needed.
  - When every receiver is a signed COMP-5 or native item, a word's
    wrapping gives the low bytes the stack's store would give.
  - Otherwise the code tests for overflow and falls back to the stack
    for the whole statement.
  - Division is allowed only at the top, since `7 / 2 * 2` is 7.
- **Decimals in registers** (`dx_*`): items of up to 18 digits, as
  64-bit scaled integers in register pairs, with the scales fixed at
  compile time.
  - This path is taken only when the stack would neither shed a digit
    nor overflow: every intermediate below 9 * 10^18 and a product's
    scale at most 18.
  - Division, at the top only, is the stack's own long division.

## 8.8.3 concatenation expressions (2002; implemented 2026-10-01, ISSUES-120)

`literal & literal` is one literal (general rule 3), so the source text
is joined once COPY and REPLACE are done, before anything parses it.

| rule | paraphrase | disposition |
|---|---|---|
| 8.8.3.1 | literal & literal, chained | **test**: 2002/concat (identical to GnuCOBOL: a VALUE, a constant entry, MOVE, DISPLAY, a relation, across lines); **refused**: bad/std2002-concat-operand (a data item), bad/concat-85 |
| 8.8.3.2 rule 1 | one class; a figurative constant either side, not ALL | **test**: 2002/concatfig (ZERO, SPACE, QUOTE; national; boolean); **refused**: bad/std2002-concat-class, bad/std2002-concat-all. HIGH-VALUE and LOW-VALUE are **gap**: their characters are the collating sequence's, not known when the text is joined |
| 8.8.3.2 rules 2-4 | the result's length | **test**: past 160 positions under -std=2002 it is BP-E20, as a long literal is; past 8,191 refused |
| 8.8.3.3 rules 1-3 | the class, the value, a literal's equivalent | **test**: 2002/concat, 2002/concatfig. GnuCOBOL refuses a figurative operand (docs/oracles.md) |

