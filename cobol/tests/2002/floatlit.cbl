identification division.
program-id. floatlit.
*> Floating-point numeric literals (2023 8.3.3.3.3; standard-queue item
*> 15): a significand with a decimal point, E, an exponent of up to four
*> digits, either signed -- worth exactly the significand times ten to
*> the exponent, so 1.5E+3 is 1500 and 1.5E-3 is 0.0015, in a VALUE, a
*> MOVE, a COMPUTE and a condition.  The exponent's range here keeps the
*> value within 31 digits (rule 3 leaves the range to the implementor).
*> 3E5, with no point, is a word, not a literal.  GnuCOBOL 4 agrees.
data division.
working-storage section.
01  a        pic 9(5)v99 value 1.5E+3.
01  b        pic 9(5)v9(5) value 1.5E-3.
01  f        usage float-long value 2.5E+2.
01  w        pic 9(31) value 1.0E+30.
01  r        pic s9(7)v99.
procedure division.
    display a " " b
    compute r = f display r
    display w
    compute r = 1.25E2 * 2 + .5E1 - 0.0E0
    display r
    if a > 1.4E3 display "gt" end-if
    move -2.5e+1 to r display r
    move 123.456e-2 to r display r
    stop run.
end program floatlit.
