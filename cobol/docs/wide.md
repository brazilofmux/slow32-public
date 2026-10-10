# Thirty-one digits

COBOL 2002 raised a numeric item's limit from 18 digits to 31 (and a
numeric literal's likewise); 2014 kept it. s32-cobc has held values as a
64-bit integer and a scale since stage 2, which covers 18 digits exactly
and nothing past them. This is the plan for the rest, started 2026-09-29
(cobol ISSUES-117).

## The shape: a wide path beside the narrow one

Nothing that fits in 18 digits changes. Every item, literal and statement
whose digits stay within 18 keeps the 64-bit code it has today -- the
-std=85 byte-identity gate over the Open Systems suite says so, and the
hot paths (inline DISPLAY decode, binary adds, the decimal stack) keep
their speed. Only an item of 19-31 digits, a literal of 19-31 digits, or
a statement whose operands need more than 18 takes the wide path. The
compiler knows every descriptor's digit count, so the choice is made at
compile time, statement by statement; the runtime never guesses.

Under -std=85 nothing reaches the wide path: 85 allows 18 digits, and
the one 85 program that needs more intermediate width (majesty's dist01,
BP-E14) keeps today's code.

## The wide value

    typedef struct { uint32_t m[4]; int neg; int scale; } cob_wnum;

A sign, a scale, and a 128-bit magnitude in four 32-bit limbs (10^38 <
2^127, so 38 digits hold: the 31 an item may have and guard digits for
the intermediate results). SLOW-32 is a 32-bit machine and clang has no
__int128 for it, so the arithmetic is written out in libcob/wide.h:
multi-limb add, subtract, compare, multiply by a small number, divide by
a small number (digit conversion in 10^9 chunks), a full multiply into
eight limbs (two 31-digit operands make 62 digits, shed back to 38 by
dropping fraction digits as the narrow multiply does), and a long
division. A host test (tests/wide_test.c) checks every operation against
the host's unsigned __int128 on random operands, as bt_test does for the
B-tree.

## Phases

1. **Items and moves.** PICTURE up to 31 digits under -std=2002; BINARY
   of 19-31 digits is 16 bytes; literals up to 31 digits; VALUE; MOVE
   between any numeric items where either side passes 18 (numeric,
   numeric-edited, alphanumeric); relation conditions; DISPLAY. The
   runtime: cob_wget / cob_wput_x (the wide cob_get_num / cob_put_num_x),
   used by cob_move, cob_cmp and cob_display_field when a descriptor
   passes 18 digits. Arithmetic on such items is refused, with that
   message, until phase 2 -- never silently truncated.
2. **Arithmetic.** A wide evaluation stack (cob_wpush, cob_wadd, cob_wsub,
   cob_wmul, cob_wdiv, cob_wneg, cob_wcmp, cob_wtop_store and the ADD TO /
   SUBTRACT FROM forms), used by ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
   and conditions when an operand or receiver passes 18 digits or the
   composite of operands does (2023 14.7.7 rule 2 allows 31; today that is
   the "31-digit gap"). ROUNDED, ON SIZE ERROR, REMAINDER, EC-SIZE.
3. **The rest.** Intrinsic functions with wide arguments or results,
   numeric-edited receivers past 18 digits, BINARY-DOUBLE (its range needs
   19 digits), SORT and SEARCH ALL keys, the numeric class test, national
   numeric items, INITIALIZE, SET, and the EC-DATA checks.

Each phase is gated like the sweeps: harness, CCVS-85, -std=85
byte-identity, majesty and majesty-functions, the Open Systems papers,
and GnuCOBOL as the oracle (it holds 38 digits).

## Intermediate rounding (2014, queue item 22)

The digits an intermediate sheds -- here, at `w_align2` (the larger
scale cut to the smaller once the smaller has no room to scale up), in
`w_mul` and `w_div` (a scale past 38; a quotient's remainder) and in
`w_fit` (a magnitude past 38 digits), and on the narrow stack in
`ndiv_core` (the quotient's remainder) and `cob_nmul` (an operand's
fraction digits shed for room) -- are truncated, the implementor's rule
for NATIVE arithmetic (2023 11.9.11 GR 1). A unit's OPTIONS
INTERMEDIATE ROUNDING clause changes that for its own statements:
`cob_iround`, set by the activation descriptor on entry and restored on
leaving, and `iround_up()` at each of those sites decides the last kept
digit from the first dropped one and whether more was dropped --
NEAREST-AWAY-FROM-ZERO, NEAREST-EVEN, or PROHIBITED, which makes any
drop a size error. The compiler's register paths truncate and are not
taught the modes: a unit with the clause takes the stack paths
(`g_nohx`, the `-fno-hot-arith` switch's flag), so the differential
between the two paths that `ccvs/both-paths` keeps does not apply to
such a unit.

## Standard-decimal arithmetic (2014, queue item 49)

`OPTIONS. ARITHMETIC IS STANDARD-DECIMAL.` (2023 8.8.1.5, 11.9.5 GR 3)
makes a unit's arithmetic decimal128's: every operand and every
intermediate is a standard-decimal intermediate data item (SDIDI) of at
most 34 significant digits, each operation rounded once by the unit's
INTERMEDIATE ROUNDING mode (NEAREST-AWAY-FROM-ZERO implied, 11.9.11 GR
3a), the exponent from -6176 to 6144, past which EC-SIZE-OVERFLOW and
EC-SIZE-UNDERFLOW are the size error condition.  Implemented 2026-10-09
(ISSUES 133) on this stack's floating mode -- the `isq` mode the standard
software floats brought, where the scale runs negative and digits are
shed for room rather than reported as a size error -- plus `sd_round`
after each operation and each push.  The compiler marks the unit
(`g_arith_sd`, bit 12 of the activation descriptor, saved and restored
with the rounding mode) and sends every arithmetic statement,
expression, comparison and numeric function of the unit down the wide
floating-decimal path; a float operand is converted exactly rather than
making the statement a double one; a function's value keeps all its
digits (fn_wresult no longer widens the integer part to 18 - fscale).

Three things make the rounding ISO/IEC 60559's and not a double one.
The inner sheds -- alignment, a product past 38 digits, a quotient's
remainder, fitting 38 -- truncate under this mode and set `sd_sticky`
when anything nonzero went, so the one rounding to 34 digits sees the
whole dropped part (`shed_up`).  The floating multiply keeps a scale
past 38 (a product below 1E-38 used to vanish in `isq` statements too).
And a comparison clears the size flag its intermediates may have set,
which otherwise leaked into the next statement's store (a native
hazard as well, now closed).  Exponentiation follows 8.8.1.5.4: x,
x*x, (x*x)*x, (x*x)*(x*x), then one multiply at a time; a negative
exponent is 1 / (x ** |n|) in SDIDI arithmetic; a fractional one is the
double's, rounded to an SDIDI.  Functions with an equivalent
arithmetic expression (MEAN, MEDIAN, VARIANCE, SUM, ...) compute it on
the stack; the transcendental ones are the double's 15 digits, as
15.4.1 allows.  PROHIBITED's inexact intermediate is EC-SIZE-TRUNCATION
(11.9.11 GR 2d/3d; it was raised as EC-SIZE-OVERFLOW, fixed for NATIVE
too).

Witness: Python's decimal module with precision 34, the mode's rounding,
Emax 6144 and Emin -6143 is decimal128 as the standard defines the
SDIDI; tests/gen/gen-stddec.py writes random programs (items of 1-31
digits, literals to 31 digits and 1E22, the four operations,
exponents 0-4, unary minus, ROUNDED, ON SIZE ERROR, comparisons, one
of the five rounding modes per seed) and their reference from that
module, and gen/stddec runs forty of them.  GnuCOBOL warns the clause
is not implemented and gcobol computes natively, so neither is an
oracle; 2014/stddec is the hand test.  Not done: STANDARD-BINARY
(refused by ruling); a function value past 38 digits (fn_wresult's
form); `**` with an exponent past 1000.

## Status

Phases 1 and 2 are done (ISSUES-117): 2002/wide1-3, and
tests/wide-differential.sh, which checks random COMPUTE statements of up
to 31 digits against GnuCOBOL (960 agree; BINARY receivers past 18 digits
are left out -- GnuCOBOL does not report a PICTURE overflow there as a
size error).

Phase 3, done: BINARY-DOUBLE [SIGNED | UNSIGNED] (2002/bindouble); the
exact intrinsic functions -- MAX, MIN, ORD-MAX, ORD-MIN, SUM, RANGE,
MIDRANGE, MOD, REM, INTEGER, INTEGER-PART, ABS, SIGN, FRACTION-PART,
NUMVAL, NUMVAL-C, NUMVAL-F -- on the wide stack, their result as wide as
the value and described at run time, and written as before for any value
that fitted the old 18-digit result; SORT keys past 18 digits (table and
file); the class test, INITIALIZE, SET, SEARCH ALL, EVALUATE and national
numeric items, which needed nothing new (2002/wide4). A wide function
result in a narrow statement sheds decimals to fit 64 bits.

Phase 3 first put every exact function on the wide stack, 85 programs
included (the old S9(9)V9(9) result of MAX, SUM, NUMVAL and the rest was
wrong past nine integer digits, free/fnwidth). That doubled the
instruction count of a COBOL 85 loop around FUNCTION MOD (the cobol/bench
kernels, profiled with `slow32 -p`; ksort four times over). Now MOD,
INTEGER, INTEGER-PART, SIGN, ORD-MAX and ORD-MIN of items and literals
of at most 18 digits keep the 64-bit code -- their integer result is
exact in 18 digits -- and the wide path itself got the common cases
first: a division whose divisor fits a limb or whose operands fit 64
bits, and digit conversion that stops when the value runs out.

The floating functions (SQRT, LOG, LOG10, the trigonometric ones,
MEAN, MEDIAN, VARIANCE, STANDARD-DEVIATION, ANNUITY, PRESENT-VALUE,
EXP, EXP10) take 31-digit arguments on the wide stack and give their
result as wide as its value, computed in double and written to 15
significant digits -- what a double holds, so EXP(LOG(5)) is 5 -- with
zeros after (fn_dres). MEAN, MEDIAN and VARIANCE of exact arguments are
exact, in decimal. Done 2026-10-06 (standard-queue item 13;
2002/widefloatfn, GnuCOBOL 4 agreeing where its exact result and the 15
digits coincide):

- SIN, COS and TAN reduce the argument by 2 pi in decimal before the
  double sees it, so SIN(10 ** 24) is sin of 10 ** 24 (-0.9964...), not
  of the nearest double (-0.5586...).
- A double becomes a wide number exactly (w_from_dbl: the mantissa and
  the binary exponent, worked in eight limbs), so a float item of 10 **
  25 stored into a 31-digit item is the double's own value,
  10000000000000000905969664; the old conversion scaled in double and
  lost the digits past its 53 bits.
- A wide number becomes a double in one rounding (w_to_dbl: 15 digits
  cut in decimal, then an exact power of ten).

Rulings: a result's precision is the double's, 15 significant digits;
the 15th is the soft libm's to lose on the interpreters (its exp, sin
and tan reach about 1e-15 relative error; the DBT runs the host's libm,
so the last digit can differ by engine: runtime ISSUES-29), and a test
shows at most 13 significant digits of such a result.

