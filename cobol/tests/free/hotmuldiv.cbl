*> MULTIPLY, DIVIDE and COMPUTE on binary integers, computed in a word
*> where that is the decimal stack's answer (default dialect: COMP-5):
*> signs, remainders, a zero and a -1 divisor, truncation, wrapping.
identification division.
program-id. hotmuldiv.
data division.
working-storage section.
01 a5  pic s9(8) comp-5.
01 b5  pic s9(8) comp-5.
01 q5  pic s9(8) comp-5.
01 r5  pic s9(8) comp-5.
01 h5  pic s9(4) comp-5.
01 u5  pic 9(4) comp-5.
01 c4  pic s9(4) comp.
01 cu  pic 9(3) comp.
01 d6  pic s9(6).
01 du  pic 9(4).
01 i   pic s9(4) comp.
01 j   pic s9(4) comp.
01 t.
   05 e pic s9(4) comp occurs 5.
01 x   pic s9(5) comp-5.
procedure division.
    move -17 to a5 move 5 to b5
    divide b5 into a5 giving q5 remainder r5 display q5 " " r5
    divide a5 by b5 giving q5 remainder r5 display q5 " " r5
    move 17 to a5 move -5 to b5
    divide b5 into a5 giving q5 remainder r5 display q5 " " r5
    move 0 to b5 move 77 to q5 move 66 to r5
    divide b5 into a5 giving q5 remainder r5 display "div0 " q5 " " r5
    move -1 to b5
    divide b5 into a5 giving q5 remainder r5 display q5 " " r5
    move 12345 to a5 move 7 to b5
    divide b5 into a5 giving c4 remainder cu display c4 " " cu
    divide b5 into a5 giving d6 du display d6 " " du
    move 1000 to c4 divide 3 into c4 display c4
    multiply 3 by c4 display c4
    multiply 50 by c4 display c4
    multiply a5 by b5 giving d6 display d6
    move -30000 to h5 multiply 3 by h5 display h5
    move 40000 to u5 multiply 2 by u5 display u5
    move 2147483000 to a5 compute x = a5 * 3 + 7 display x
    compute q5 = 400 * a5 + 100 * b5 - 4 display q5
    compute c4 = (b5 + 3) * (b5 - 10) display c4
    compute cu = b5 - 20 display cu
    compute d6 = -(b5 * b5) + 1 display d6
    compute q5 = (a5 - 5) / -b5 display q5
    compute q5 rounded = 7 / 2 display q5
    compute q5 = 7 / 2 * 2 display q5
    move 1 to i
    perform until i > 5
        compute e(i) = i * i - 3
        add 1 to i
    end-perform
    move 2 to j
    compute x = e(j + 1) * e(5) / e(j) display x
    display e(1) " " e(2) " " e(3) " " e(4) " " e(5)
    stop run.
