identification division.
program-id. fpedited.
*> Floating-point numeric-edited items (2023 13.18.40.3 rule 13b,
*> 14.6.8.4; standard-queue item 15): the PICTURE a significand, E, and
*> an exponent of '+' and one to four 9s; a value is placed with the
*> significand's first digit not zero, truncated to its positions
*> (ROUNDED rounds), the exponent signed; zero is zeros and E+0; a
*> 31-digit value and a float go the same way; the item read back is
*> the significand times ten to the exponent.  No oracle: GnuCOBOL 4
*> rejects the E at run time.
data division.
working-storage section.
01  e1       pic +9.9(5)E+99.
01  e2       pic -9(2).9(3)E+9.
01  e3       pic 9.99E+999.
01  v        pic s9(7)v9(4) value -1234.5678.
01  w        pic 9(25) value 1234567890123456789012345.
01  f        usage float-long.
01  r        pic s9(9)v9(5).
procedure division.
    move v to e1 display "[" e1 "]"
    move v to e2 display "[" e2 "]"
    move 0.00012345 to e1 display "[" e1 "]"
    move 0 to e1 display "[" e1 "]"
    move 123 to e3 display "[" e3 "]"
    move w to e1 display "[" e1 "]"
    move 123456.789 to f
    move f to e1 display "[" e1 "]"
    move e1 to r display "back " r
    move e2 to r display "back " r
    compute e1 rounded = 2 / 3 display "[" e1 "]"
    display function length(e1) " " function length(e3)
    stop run.
end program fpedited.
