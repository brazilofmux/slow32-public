*> 31 digits, phase 1 (COBOL 2002; docs/wide.md): items of 19-31 digits
*> as DISPLAY, BINARY and PACKED-DECIMAL, signed and not, with fractions;
*> VALUE with long literals; MOVE between them and to and from narrower
*> items, numeric-edited and alphanumeric items (high-order and fraction
*> truncation, sign handling); relation conditions across widths and
*> scales; DISPLAY of each usage.
identification division.
program-id. wide1.
data division.
working-storage section.
01 a  pic 9(31) value 1234567890123456789012345678901.
01 b  pic s9(25)v9(6) value -1234567890123456789.123456.
01 c  pic s9(31) binary.
01 d  pic s9(29)v99 packed-decimal.
01 n9 pic 9(31) value 9999999999999999999999999999999.
01 e  pic 9(5).
01 f  pic s9(20)v9(2) sign leading separate.
01 g  pic 9(20).
01 h  pic -(29)9.99.
01 i  pic zzz,zzz,zzz,zzz,zzz,zzz,zzz,zz9.
01 x  pic x(40).
01 y  pic x(35) value "  -123456789012345678901234.5678".
01 s1 pic s9(18) value -999999999999999999.
procedure division.
    display "a " a
    display "b " b
    move a to c display "c " c
    move b to d display "d " d
    move -1 to c display "c " c
    move n9 to c display "c " c
    move c to x display "x [" x "]"
    move 42 to c display "c " c
    move c to e display "e " e
    move a to e display "e " e
    move a to g display "g " g
    move b to f display "f " f
    move b to h display "h [" h "]"
    move a to i display "i [" i "]"
    move y to d display "d " d
    move s1 to c display "c " c
    move c to s1 display "s1 " s1
    move zero to c display "c " c
    if a > c display "a > c" end-if
    if b < 0 display "b < 0" end-if
    if n9 > a display "n9 > a" end-if
    move a to c
    if c = a display "c = a" end-if
    move 1234567890123456789012345678.901 to d display "d " d
    if d < a display "d < a" end-if
    if d > 1234567890123456789012345678.90 display "d > lit" end-if
    if s1 < b display "s1 < b" else display "s1 >= b" end-if
    stop run.
