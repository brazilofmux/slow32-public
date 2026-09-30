*> Micro Focus's COMP-X rules (docs/usage.md; default dialect): a negative
*> value MOVEd in is stored in two's complement, and with ON SIZE ERROR
*> a 9(n) item's digits decide the size error, though it stores by its
*> capacity.  GnuCOBOL differs (.oracle-expected, docs/oracles.md).
identification division.
program-id. compxmf.
data division.
working-storage section.
01 x2  pic xx comp-x.
01 n2  pic 9(2) comp-x.
01 raw redefines n2 pic x.
01 b   pic 999.
procedure division.
    move -1 to x2 display "move -1 to xx comp-x: " x2
    move -3 to n2 display "move -3 to 9(2) comp-x: " n2
    move 90 to n2
    add 50 to n2 on size error display "size error (99 exceeded)" not on size error display "no size error: " n2 end-add
    add 50 to n2 display "without the phrase: " n2
    stop run.
