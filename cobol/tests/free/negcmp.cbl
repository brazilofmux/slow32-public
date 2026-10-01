identification division.
program-id. negcmp.
*> A comparison with a negative numeric literal whose integer digits
*> outnumber the item's: the literal's value, sign and all (X3.23-1985
*> VI-55, the comparison of numeric operands: algebraic value).  The
*> oracle reads such a literal without its sign (docs/oracles.md); found
*> by the differential generator, tests/gen.
data division.
working-storage section.
01 n00 pic s9(4)v9(3).
01 n06 pic 9(4).
01 m pic s9(6) value -316940.
procedure division.
    move 9884.108 to n00
    if n00 >= -316940 display "1 T" else display "1 F" end-if
    if n00 >= m display "2 T" else display "2 F" end-if
    if n00 >= - 316940 display "3 T" else display "3 F" end-if
    if n00 > -316940 display "4 T" else display "4 F" end-if
    if n00 = -316940 display "5 T" else display "5 F" end-if
    move 9431 to n06
    if n06 < -59501.44 display "6 T" else display "6 F" end-if
    if n06 < -5 display "7 T" else display "7 F" end-if
    if n06 > -5 display "8 T" else display "8 F" end-if
    stop run.
