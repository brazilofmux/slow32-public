*> DIVIDE ... GIVING an item that is also the divisor, or the dividend,
*> with REMAINDER: the remainder is of the operands as they were before
*> the quotient was stored over one of them.  The numeric stack's path
*> (packed and DISPLAY operands) worked the remainder out again after the
*> store, from the quotient where the operand had been: 10 / 4 left 0.
identification division.
program-id. divremgiving.
data division.
working-storage section.
01  g.
    05  p   pic s9(6) packed-decimal.
    05  b   pic s9(6) comp.
    05  d   pic s9(6).
    05  v   pic s9(4)v99 packed-decimal.
77  w   pic 9(9).
77  n   pic 9(9) comp.
77  r   pic 9(4).
77  rv  pic 9(2)v99.
01  t.
    05  e   pic 9(4) packed-decimal occurs 3.
77  i   pic 9 value 2.
procedure division.
    move 4 to p b d  move 10 to w n
    divide p into w giving p remainder r
    display "divisor packed   " p " " r
    divide b into n giving b remainder r
    display "divisor binary   " b " " r
    divide d into w giving d remainder r
    display "divisor display  " d " " r
    move 4 to p  move 11 to d
    divide p into d giving d remainder r
    display "dividend display " d " " r
    move 11 to p  move 4 to d
    divide d into p giving p remainder r
    display "dividend packed  " p " " r
    move 11 to p
    divide p by 4 giving p remainder r
    display "BY, dividend     " p " " r
    move 4 to p
    divide 11 by p giving p remainder r
    display "BY, divisor      " p " " r
    move 2.50 to v
    divide v into 9 giving v remainder rv
    display "decimals         " v " " rv
    move 3 to p
    divide p into p giving p remainder r
    display "both             " p " " r
    move 4 to e(2)  move 10 to w
    divide e(i) into w giving e(i) remainder r
    display "a table element  " e(2) " " r
    move 4 to p  move 10 to w
    divide p into w giving p remainder r
        on size error display "size error"
    end-divide
    display "with the phrase  " p " " r
    move 0 to p  move 77 to r
    divide p into w giving p remainder r
        on size error display "zero divisor"
    end-divide
    display "zero             " p " " r
    stop run.
