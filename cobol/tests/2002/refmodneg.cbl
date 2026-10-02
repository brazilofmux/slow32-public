*> A reference modification's length is an arithmetic expression, and
*> an expression's value is not its intermediate results': 8 - 9 + 2 is
*> 1, though 8 - 9 is negative and 9 is in an unsigned item.  GnuCOBOL 4
*> computes it in the unsigned operand's type when one is BINARY and
*> unsigned: the difference wraps, and the part runs on past the item
*> to the receiver's length (.oracle-expected; docs/oracles.md).  The
*> same in a leftmost position or a subscript is 2002/refmodnegp.
identification division.
program-id. refmodneg.
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
    move x(2:a - b + c) to w
    display "length, unsigned binary:   [" w "]"
    move spaces to w
    move x(2:a - d + c) to w
    display "length, signed binary:     [" w "]"
    stop run.
