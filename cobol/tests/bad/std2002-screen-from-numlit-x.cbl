identification division.
program-id. p-std2002-screen-from-numlit-x.
*> FROM a numeric literal needs a numeric or numeric-edited PICTURE (13.18.25.3 rule 4).
data division.
working-storage section.
01 n pic 99 value 3.
01 sn pic s99 value 3.
01 x pic x(4) value "abcd".
01 tbl.
   05 el pic x(2) occurs 2 times.
screen section.
01 s.
   05 line 1 column 1 pic x(4) from 12.
procedure division.
    display s.
    stop run.
