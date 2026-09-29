*> MOVE's general rules for elementary moves (X3.23-1985 VI-104 rules
*> 3-5; 2023 14.9.25.4 rules 4-7): an operational sign is not moved to
*> an alphanumeric item, a separate one not even counted; a numeric-edited
*> sender is de-edited into a numeric receiver; an alphanumeric sender is
*> an unsigned integer, its rightmost digits kept; ZERO is numeric to a
*> numeric-edited item; a group is moved as bytes, no conversion.
*> docs/conformance/move.md
identification division.
program-id. moverules.
data division.
working-storage section.
01 sn pic s9(3) value -123.
01 ss pic s9(3) sign leading separate value -123.
01 x6 pic x(6).
01 ed pic -zz9.99 value "- 12.50".
01 n52 pic s9(3)v99.
01 big pic x(20) value "12345678901234567890".
01 n18 pic 9(18).
01 ne pic zz9.
01 n3 pic 9(3).
01 grp.
   05 gd pic 9(2) value 42.
   05 gx pic x value "7".
01 gn pic 9(3).
procedure division.
    move sn to x6 display "signed [" x6 "]"
    move ss to x6 display "separate [" x6 "]"
    move ed to n52 display "de-edited " n52
    move big to n18 display "rightmost " n18
    move zero to ne display "zero [" ne "]"
    move all "7" to n3 display "all " n3
    move grp to gn display "group [" gn "]"
    stop run.
