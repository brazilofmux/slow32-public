identification division.
program-id. p-std2002-screen-fromto-lit.
*> FROM literal with TO: nothing to key (13.17.2).
data division.
working-storage section.
01 n pic 99 value 3.
01 sn pic s99 value 3.
01 x pic x(4) value "abcd".
01 tbl.
   05 el pic x(2) occurs 2 times.
screen section.
01 s.
   05 line 1 column 1 pic x(4) from "abcd" to x.
procedure division.
    accept s.
    stop run.
