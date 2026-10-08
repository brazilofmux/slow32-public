identification division.
program-id. p-std2002-screen-occurs-noelem.
*> OCCURS on a FROM/TO/USING item: a table element written without its subscript (13.18.38.3 rule 13).
data division.
working-storage section.
01 n pic 99 value 3.
01 sn pic s99 value 3.
01 x pic x(4) value "abcd".
01 tbl.
   05 el pic x(2) occurs 2 times.
screen section.
01 s.
   05 line 1 column 1 value "h".
   05 column plus 1 pic x(4) using x occurs 2 times.
procedure division.
    display s.
    stop run.
