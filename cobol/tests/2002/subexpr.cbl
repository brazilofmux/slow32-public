*> Arithmetic-expression subscripts (2002 8.4.1.2.1): beyond COBOL 85's
*> integer, data-name and data-name +/- integer -- found by the X-COBOL
*> compile survey (E(129 - I), K(I - N, 1)).  Subscripted subscripts,
*> numeric-display and binary operands, two dimensions, a computed
*> reference-modification start beside them, receiving operands, and a
*> PERFORM VARYING over one.
identification division.
program-id. subexpr.
data division.
working-storage section.
01 t.
   05 e pic 99 occurs 9.
01 m.
   05 r occurs 3.
      10 c pic x(4) occurs 4.
01 ix.
   05 k pic 9 occurs 5.
01 i pic 99 value 3.
01 n binary-long value 1.
01 j pic 9.
01 s pic x(12) value "abcdefghijkl".
01 w pic x(12).
procedure division.
    move zero to t
    move 7 to e(9 - i)
    move 5 to e(i - n)
    move 4 to e(i * 2)
    display e(6) " " e(2) " " e(9 - i) " " e((i + 1) / 2)
    *> a subscripted subscript
    move 1 to k(1) move 4 to k(2) move 2 to k(3)
    move 9 to e(k(2))
    display e(k(2)) " " e(k(k(3)) + 2)
    *> two dimensions, both computed
    move "abcd" to c(1, 1)
    move "wxyz" to c(i - 1, n + 3)
    display c(2, 4) " " c(i - 2, n * 1)
    *> with a computed reference-modification start
    move s to w
    display c(i - 1, 2 + 2)(i - 1:2) " " w(i * 2:3)
    *> receiving operands of COMPUTE and ADD
    compute e(i + 4) = e(i - 1) + 1
    add 10 to e(9 - n)
    display e(7) " " e(8)
    *> a PERFORM VARYING over one
    move zero to t
    perform varying j from 1 by 1 until j > 4
        move j to e(10 - j * 2)
    end-perform
    display t
    stop run.
