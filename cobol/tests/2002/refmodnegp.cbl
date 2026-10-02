*> As 2002/refmodneg, in a leftmost position and in a subscript: 8 - 9
*> + 2 is 1 there too.  No oracle: GnuCOBOL 4 computes these in the
*> unsigned operand's type as well, and the wrapped position is an
*> invalid address (SIGSEGV) -- docs/oracles.md.
identification division.
program-id. refmodnegp.
data division.
working-storage section.
01  g.
    05  x        pic x(10) value "ABCDEFGHIJ".
    05  y        pic x(10) value "0123456789".
01  tb.
    05  t        occurs 4 times pic x(2).
01  w            pic x(10).
01  a            pic s9(7) packed-decimal value 8.
01  b            pic 9(9) binary value 9.
01  c            pic 9(9) value 2.
01  d            pic s9(9) binary value 9.
01  e            pic 9(4) binary value 8.
procedure division.
    move "aabbccdd" to tb
    move spaces to w
    move x(e - b + c:3) to w
    display "position, unsigned binary: [" w "]"
    move spaces to w
    move t(e - b + c) to w
    display "subscript, unsigned binary: [" w "]"
    move spaces to w
    move x(e - b + 4:e - b + 3) to w
    display "both, all binary:          [" w "]"
    stop run.
