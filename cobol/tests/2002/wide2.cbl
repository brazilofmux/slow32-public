*> 31 digits, phase 2 (COBOL 2002; docs/wide.md): ADD, SUBTRACT,
*> MULTIPLY, DIVIDE and COMPUTE with operands and receivers past 18
*> digits -- ROUNDED, ON SIZE ERROR, REMAINDER, negative values, mixed
*> scales, a product of two 18-digit items, a composite of 19-31 digits
*> built from narrow items, relations on expressions, VARYING a wide item.
*> Exponentiation: wide3.
identification division.
program-id. wide2.
data division.
working-storage section.
01 a  pic s9(31) value 1234567890123456789012345678901.
01 b  pic s9(25)v9(6) value -1234567890123456789.123456.
01 c  pic s9(31).
01 d  pic s9(29)v99.
01 e  pic s9(18) value 999999999999999999.
01 f  pic s9(18) value 123456789012345678.
01 g  pic s9(31) packed-decimal.
01 h  pic s9(31) binary.
01 r  pic s9(31).
01 q  pic s9(20).
01 s  pic 9(5).
01 t  pic s9(10)v9(12) value 0.000000000001.
01 u  pic s9(12)v9(9).
01 k  pic 9(21).
procedure division.
    move a to c
    add 1 to c display "add1 " c
    add b b giving d display "add2 " d
    subtract e from c display "sub " c
    multiply e by f giving c display "mul " c
    compute g = e * f + 1 display "cmp1 " g
    compute h = (a - 1) / 9 display "cmp2 " h
    divide a by 7 giving q remainder r display "div " q " " r
    compute d rounded = a / 3 display "rnd " d
    compute d = a / 3 display "trn " d
    compute c = a * 10 on size error display "size error" not on size error display "no size error" end-compute
    add a to a giving c on size error display "size error 2" end-add
    compute u = t + 123456789012 display "gap " u
    compute s = a / a display "one " s
    if a + 1 > a display "rel yes" end-if
    if b * 2 < b display "rel neg" end-if
    move 999999999999999999999 to k
    add 1 to k display "k " k
    perform varying k from 1 by 1000000000 until k > 3000000000
        display "vary " k
    end-perform
    compute c = - a display "neg " c
    compute d rounded = 2 / 3 display "rnd2 " d
    compute d = 2 / 3 display "trn2 " d
    stop run.
