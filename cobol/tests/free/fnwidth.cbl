*> Numeric intrinsic functions of values past nine integer digits in a
*> COBOL 85 program: MAX, SUM and NUMVAL of 12-digit values.  Their
*> results were an S9(9)V9(9) buffer, and these came back as garbage
*> (223372036 for 123456789012) until ISSUES-117 gave the exact functions
*> a result as wide as the value.  MOD, INTEGER and INTEGER-PART of
*> items keep the 64-bit code, whose 18-digit integer result is exact.
*> docs/wide.md
identification division.
program-id. fnwidth.
data division.
working-storage section.
01 a pic 9(12) value 123456789012.
01 b pic 9(12) value 5.
01 c pic 9(15).
01 d pic s9(14)v9(4).
01 e pic s9(18).
procedure division.
    move function max(a b) to c display "max " c
    move function min(a b) to c display "min " c
    move function numval("123456789012") to c display "numval " c
    move function numval("-98765432109876.5432") to d display "numval2 " d
    compute c = function integer(a) + 1 display "int " c
    move function sum(a b) to c display "sum " c
    compute c = function mod(a 7) display "mod " c
    compute c = function max(a b) * 3 display "max*3 " c
    move -98765432109876.5 to d
    compute e = function integer(d) display "intneg " e
    compute e = function integer-part(d) display "intpart " e
    compute e = function mod(-123456789012 7) display "modneg " e
    compute e = function mod(123456789012345678 -1000) display "mod18 " e
    stop run.
