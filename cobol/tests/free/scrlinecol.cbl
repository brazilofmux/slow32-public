*> Where an entry goes when a clause is left out (2023 13.18.14.4 rules
*> 16-17, 13.18.35.4 rule 13):
*>   - LINE without COLUMN: column 1 of that line -- X, though the item
*>     before ends on the same line (it used to follow that item);
*>   - neither: the line before, immediately after the item before -- Y;
*>   - COLUMN without LINE: the line before -- Z at column 10.
*> No oracle: screens need a real tty.
identification division.
program-id. scrlinecol.
data division.
working-storage section.
screen section.
01  s1.
    05  line 2 col 5 value 'ab'.
    05  line 2 value 'X'.
    05  value 'Y'.
    05  col 10 value 'Z'.
procedure division.
    display s1
    stop run.
