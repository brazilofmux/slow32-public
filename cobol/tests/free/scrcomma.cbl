*> DECIMAL-POINT IS COMMA in a screen's numeric-edited field: the comma
*> is the decimal point's key and its column (the point was looked for
*> as a period, so the cursor stood in the wrong place).  The keys come
*> from scrcomma.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrcomma.
environment division.
configuration section.
special-names.
    decimal-point is comma.
data division.
working-storage section.
01  amt pic 9(3)v99 value 0.
screen section.
01  s1.
    05  line 1 column 1 pic zz9,99 using amt.
procedure division.
    display s1
    accept s1
    display amt at 0301
    stop run.
