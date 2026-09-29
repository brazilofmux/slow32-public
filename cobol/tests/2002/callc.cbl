*> A C function called with ten BY VALUE arguments -- the ninth and
*> tenth on the stack, where the C ABI looks for them -- and its r1
*> result taken by RETURNING under -std=2002, where RETURNING also serves
*> COBOL programs (tests/c/callc.c).  No oracle: the C side is SLOW-32's.
*> docs/conformance/call.md
identification division.
program-id. callc.
data division.
working-storage section.
01 r   binary-long.
01 x   binary-long value 10.
procedure division.
    call "weigh10" using by value 1 2 3 4 5 6 7 8 9 x returning r
    display "weigh10 = " r
    stop run.
