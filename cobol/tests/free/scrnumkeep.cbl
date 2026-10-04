*> A key a numeric field refuses leaves its value alone: a decimal point
*> on an integer picture, a sign on an unsigned one.  (The refused key
*> used to clear the digits behind a screen that still showed them, and
*> Enter stored zero.)  Then a key it takes replaces the value, as the
*> first key does.  The keys come from scrnumkeep.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrnumkeep.
data division.
working-storage section.
01  a pic 9(3) value 42.
01  b pic 9(3) value 17.
01  c pic 9(3) value 5.
screen section.
01  s1.
    05  line 1 column 1 pic 9(3) using a.
    05  line 2 column 1 pic 9(3) using b.
    05  line 3 column 1 pic 9(3) using c.
procedure division.
    display s1
    accept s1
    display a at 0501 ' ' b ' ' c
    stop run.
