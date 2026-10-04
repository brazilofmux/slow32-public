*> Insert mode shows in the cursor (the term service's cursor style,
*> docs/SPEC.md 8.14.5): Insert turns the cursor into a bar, Insert
*> again, or the end of the ACCEPT, gives the terminal its own back.
*> ab, Left, Insert, X: aXb.  Tab into the numeric field, which has no
*> insert mode: the terminal's cursor.  The keys come from
*> scrinsert.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrinsert.
data division.
working-storage section.
01  a pic x(5) value spaces.
01  n pic 9(3) value 0.
screen section.
01  s1.
    05  line 1 column 1 pic x(5) using a.
    05  line 2 column 1 pic 9(3) using n.
procedure division.
    display s1
    accept s1
    display '[' at 0401 a '] ' n
    stop run.
