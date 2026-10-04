*> Clauses of a screen item that shape its input (2023 13.18):
*>   a  S9(3) SIGN LEADING SEPARATE: the sign has a column of its own,
*>      left of the digits; 4 2 - is -42
*>   b  S9(3) SIGN TRAILING SEPARATE: the same, on the right; from -7
*>      the + key makes it 7
*>   c  X(6) JUSTIFIED: what is keyed goes to the right of the item when
*>      the cursor leaves the field
*>   d  ZZ9 FULL: Tab is refused while a digit position is suppressed
*>      (12), accepted when none is (123) -- and zero would do too
*>   e  ZZ9.99 TO: starts as zeros through its picture, not as spaces
*>      (14.9.1.4 rule 13); Enter leaves it
*> The keys come from scrclauses.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrclauses.
data division.
working-storage section.
01  a pic s9(3) value 0.
01  b pic s9(3) value -7.
01  c pic x(6) value spaces.
01  d pic 9(3) value 0.
01  e pic 9(3)v99 value 1.5.
01  n-out pic -(3)9.
screen section.
01  s1.
    05  line 1 column 1 pic s9(3) using a sign leading separate.
    05  line 2 column 1 pic s9(3) using b sign is trailing separate character.
    05  line 3 column 1 pic x(6) using c justified right.
    05  line 4 column 1 pic zz9 using d full.
    05  line 5 column 1 pic zz9.99 to e.
procedure division.
    display s1
    accept s1
    move a to n-out
    display n-out at 0701
    move b to n-out
    display n-out at 0711 ' [' c '] ' d ' ' e
    stop run.
