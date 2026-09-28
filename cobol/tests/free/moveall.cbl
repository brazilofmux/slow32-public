identification division.
program-id. moveall.
*> MOVE ALL literal to a numeric or numeric-edited item (cobol ISSUES-47).
*> The literal is repeated to the receiver's character positions, then
*> moved as an alphanumeric literal is: an unsigned integer aligned on
*> the decimal point (X3.23-1985 IV-11).  The 1985 text's own example,
*> XVII-82 (X3J4 interpretation B-23): ALL "99" and ALL "123" to a
*> PIC 99V99 item give 99.00 and 31.00.  A literal longer than one
*> character is obsolete element 2 (BP-O9).  GnuCOBOL differs from the
*> text on the two multi-digit lines that are not all one digit: 12.00
*> for ALL "123" and 21212 for ALL "12" to 9(5) (the text: "12121", the
*> integer 12121).  Its values are in moveall.oracle-expected.
data division.
working-storage section.
01  a               pic 99v99.
01  b               pic 9(5).
01  e               pic zz9.99.
01  s               pic s999v9 sign leading separate.
procedure division.
main.
    move all "99" to a      display a
    move all "123" to a     display a
    move all "1" to a       display a
    move all "12" to b      display b
    move all "7" to e       display e
    move all "123" to e     display e
    move all "5" to s       display s
    stop run.
