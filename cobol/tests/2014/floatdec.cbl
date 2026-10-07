*> FLOAT-DECIMAL-16 and FLOAT-DECIMAL-34 (COBOL 2014; 2023 13.18.60.4 rules
*> 17-18): IEEE decimal64 and decimal128, the values shown through PICTURE
*> receivers (DISPLAY of the items themselves is the implementor's form:
*> floatbin, no oracle).  Decimal arithmetic is exact where a binary
*> float is not: 0.1 + 0.2 is 0.3, 1.1 * 1.1 is 1.21; a quotient to the
*> format's digits; VALUE clauses; MOVE between the formats and to and
*> from PICTURE items; comparisons across formats; the sign and class
*> conditions; the default BID encoding's bytes.  GnuCOBOL agrees (a
*> size error past the format: floatbin, where GnuCOBOL stores an infinity).
*> docs/conformance/usage.md
identification division.
program-id. floatdec.
data division.
working-storage section.
01 d16 usage float-decimal-16 value 0.1.
01 d34 usage float-decimal-34 value 1.5.
01 p17 pic 9(17).
01 p5 pic 9(3)v9(5).
01 p16 pic 9(10)v9(16).
01 p34 pic 9(5)v9(26).
01 ps pic -9(3).9(5).
01 x8 pic x(8).
01 dx redefines x8 usage float-decimal-16.
procedure division.
    compute d16 = d16 + 0.2
    move d16 to p5 display "0.1 + 0.2 = " p5
    compute d16 = 1.1 * 1.1
    move d16 to p5 display "1.1 * 1.1 = " p5
    compute d16 = 1 / 3
    move d16 to p16 display "1 / 3 (16) = " p16
    compute d34 = 1 / 3
    move d34 to p34 display "1 / 3 (34) = " p34
    compute d34 = d34 * 3
    move d34 to p34 display "times 3    = " p34
    move 123.456 to d16
    move d16 to p5 display "move 123.456 = " p5
    move -2.5 to d34
    move d34 to ps display "move -2.5 = " ps
    if d34 is negative display "negative" end-if
    if d34 < d16 display "d34 < d16" end-if
    move d34 to d16
    if d16 = d34 display "equal after move" end-if
    move 0 to d16
    if d16 is zero display "zero" end-if
    if d16 is numeric display "numeric" end-if
    move 1 to dx
    if x8 = x"010000000000c031" display "BID bytes of 1, low order first" end-if
    compute d34 = 10 ** 300 * 10 ** 100
        on size error display "size error past decimal128"
        not on size error display "decimal128 holds 1E+400"
    end-compute
    move 9999999999999999 to d16
    add 1 to d16
    move d16 to p17 display "16 nines + 1 = " p17
    stop run.
