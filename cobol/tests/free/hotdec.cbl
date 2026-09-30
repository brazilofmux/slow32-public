*> Decimal arithmetic in registers (default dialect: COMP-5): packed,
*> DISPLAY and binary operands with scales, ROUNDED both signs, edited
*> receivers, division scales, a zero divisor.  Must equal the stack's.
identification division.
program-id. hotdec.
data division.
working-storage section.
01 p1  pic s9(5)v99 comp-3 value 123.45.
01 p2  pic s9(7)v9(4) comp-3 value -0.0005.
01 d1  pic s9(9)v99 value 1000.
01 d2  pic 9(5)v999 value 2.5.
01 b1  pic s9(4)v9 comp value -12.5.
01 b5  pic s9(8) comp-5 value 7.
01 r1  pic s9(5)v99.
01 r2  pic s9(3).
01 r3  pic s9(11)v9(6) comp-3.
01 r4  pic 9(3)v9.
01 ed  pic --,--9.99.
01 z   pic s9(3) value 0.
01 big pic s9(17) comp-3 value 12345678901234567.
procedure division.
    compute r1 rounded = p1 / 7 display r1
    compute r1 = p1 / 7 display r1
    compute r1 rounded = -p1 / 7 display r1
    compute r2 rounded = 2.5 display r2
    compute r2 rounded = -2.5 display r2
    compute r2 rounded = p2 * 1000 display r2
    compute r3 = p1 * d2 - b1 / 3 display r3
    compute r3 rounded = (p1 + d1) * (d2 - 0.001) display r3
    compute r4 = p1 + b1 display r4
    compute ed = d1 * -1.5 + p2 display ed
    add p1 d2 to r1 r3 display r1 " " r3
    add p1 to p1 r1 display p1 " " r1
    subtract b1 p2 from r1 r3 rounded display r1 " " r3
    subtract d2 from p1 giving r1 r2 rounded display r1 " " r2
    add b5 1.5 giving r4 rounded display r4
    multiply 1.5 by p1 r1 rounded display p1 " " r1
    multiply p1 by d2 giving r3 ed display r3 " " ed
    divide 3 into p1 r1 display p1 " " r1
    divide p1 by 7 giving r3 display r3
    divide d2 into d1 giving r1 rounded display r1
    move 77 to r1
    compute r1 = p1 / z display "z " r1
    divide z into p1 giving r1 display "z " r1
    compute r3 = big / 1000 display r3
    compute r3 = big * 0.01 display r3
    compute r2 = b5 * 3 / 2 display r2
    stop run.
