*> DECIMAL-POINT IS COMMA and DISPLAY of a number with decimal places:
*> the comma whatever the item's usage.  A COMP-3 or COMP item was
*> displayed with a period where a DISPLAY item showed the comma.
identification division.
program-id. dpcommausage.
environment division.
configuration section.
special-names.
    decimal-point is comma.
data division.
working-storage section.
01  g.
    05  d   pic s9(5)v99 value -123,45.
    05  p   pic s9(5)v99 packed-decimal value -123,45.
    05  b   pic s9(5)v99 comp value -123,45.
    05  u   pic 9(3)v9 packed-decimal value 12,5.
    05  f   pic v999 packed-decimal value 0,125.
77  s   pic s9(7)v99 packed-decimal value 1,5.
procedure division.
    display d " " p " " b " " u " " f " " s
    add 0,25 to d p b s
    display d " " p " " b " " s
    display g(1:7)
    stop run.
