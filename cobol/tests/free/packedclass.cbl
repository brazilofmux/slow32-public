*> The NUMERIC class test on a packed-decimal item looks at its bytes:
*> digit nibbles 0-9 and a sign nibble A-F.  Before the arithmetic sweep
*> (ISSUES-101) any packed item tested NUMERIC.  COMP-3 and X"..." are
*> extensions in 85: the oracle compiles it in its default dialect.
identification division.
program-id. packedclass.
data division.
working-storage section.
01 pk   pic s9(3) comp-3.
01 pkr  redefines pk pic x(2).
01 pu   pic 9(3) comp-3.
01 pur  redefines pu pic x(2).
procedure division.
    move x"123C" to pkr
    if pk is numeric display "123C numeric" else display "123C not numeric" end-if
    move x"123D" to pkr
    if pk is numeric display "123D numeric" else display "123D not numeric" end-if
    move x"12AC" to pkr
    if pk is numeric display "12AC numeric" else display "12AC not numeric" end-if
    move x"1234" to pkr
    if pk is numeric display "1234 numeric" else display "1234 not numeric" end-if
    move x"123F" to pur
    if pu is numeric display "123F numeric" else display "123F not numeric" end-if
    stop run.
