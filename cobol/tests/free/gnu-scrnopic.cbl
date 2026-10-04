*> FROM, TO and USING screen items with no PICTURE, taking their items'
*> (BP-G5, -dialect=gnucobol only): a numeric FROM is edited as its
*> item is, a USING field is as wide as its item and its keys reach it,
*> and a subscripted element takes the element's.  ACAS's sys002 (cobol
*> ISSUES-124) writes "03 using SL-VAT-Printed col 37."  The keys come
*> from gnu-scrnopic.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. gnu-scrnopic.
data division.
working-storage section.
01  flag   pic x     value 'N'.
01  amt    pic 9(3)  value 42.
01  grp.
    03  nm pic xx occurs 2.
screen section.
01  s1.
    03  value "["          line 1 col 1.
    03  using flag         col 2.
    03  value "]"          col 3.
    03  from amt           col 5.
    03  from nm (2)        col 9.
procedure division.
    move 'AB' to nm (1) move 'CD' to nm (2)
    display s1
    accept s1
    display flag at 0301
    stop run.
