identification division.
program-id. mulwide.
*> MULTIPLY whose product passes 18 digits though no item does.  The
*> result is exact, truncated only as it is stored; the stack used to
*> shed the operands' fraction digits to fit 64 bits and gave
*> 20074490570.063342 (found by the differential generator, tests/gen).
data division.
working-storage section.
01 a    pic sv9(5).
01 b    pic 9(12)v9(4) usage packed-decimal.
01 r    pic s9(12)v9(6) usage packed-decimal.
01 r2   pic 9(12)v9(4) usage packed-decimal.
01 c    pic 9(9)v9(9).
01 d    pic 9(9)v9(9).
01 r3   pic 9(9)v9(9).
procedure division.
*> 0.05565 x 361051988670.2040 = 20092543169.4968526
    move 0.05565 to a  move 361051988670.2040 to b
    multiply a by b giving r
        on size error display "size error"
    end-multiply
    display "giving " r
*> format 1, the product into the second operand
    move 361051988670.2040 to r2
    multiply a by r2
    display "by     " r2
*> 123456789.123456789 x 0.000000002 = 0.246913578246913578
    move 123456789.123456789 to c  move 0.000000002 to d
    multiply c by d giving r3
    display "small  " r3
    stop run.
