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

## Status

Phases 1 and 2 are done (ISSUES-117): 2002/wide1-3, and
tests/wide-differential.sh, which checks random COMPUTE statements of up
to 31 digits against GnuCOBOL (960 agree; BINARY receivers past 18 digits
are left out -- GnuCOBOL does not report a PICTURE overflow there as a
size error). Phase 3 is next.

