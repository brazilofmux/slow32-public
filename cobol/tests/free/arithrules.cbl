*> The arithmetic statements' common rules (2023 14.7.7 rule 4, NOTE 3;
*> X3.23-1985 6.4.4): a receiver is identified as it is reached (ADD 1
*> TO i t (i) adds to the new i's element; COMPUTE likewise), a sender at
*> the start; with ON SIZE ERROR only the receiver that overflows is left
*> unchanged; the REMAINDER comes from the unrounded quotient; a sender
*> that is also the receiver is still defined.  docs/conformance/arithmetic.md
identification division.
program-id. arithrules.
data division.
working-storage section.
01 i pic 9 value 1.
01 t.
   05 te pic 99 occurs 5 value 10.
01 a pic 99 value 7.
01 b pic 9 value 5.
01 c pic 99 value 90.
01 d pic 99 value 1.
01 q pic 99.
01 r pic 99.
01 q2 pic 9v9.
01 r2 pic 9v99.
procedure division.
    add 1 to i te(i)
    display "1: i=" i " t=" t
    move 1 to i  move all "10" to t
    compute i te(i) = 3
    display "2: i=" i " t=" t
    move 2 to i  move all "10" to t
    add te(i) to i
    display "3: i=" i
    move 5 to b  move 90 to c
    add 7 to b c d
        on size error display "4: size error"
    end-add
    display "4: b=" b " c=" c " d=" d
    move 3 to b  move 90 to c
    multiply b by a c on size error display "5: size error" end-multiply
    display "5: a=" a " c=" c
    divide 7 into 45 giving q remainder r
    display "6: q=" q " r=" r
    divide 3.4 into 7 giving q2 rounded remainder r2
    display "7: q2=" q2 " r2=" r2
    subtract 1 2 from c giving q r
    display "8: q=" q " r=" r
    move 21 to a
    add a to a
    display "9: a=" a
    stop run.
