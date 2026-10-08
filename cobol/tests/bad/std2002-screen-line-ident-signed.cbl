identification division.
program-id. p-std2002-screen-line-ident-signed.
*> LINE identifier-1 is an unsigned integer (13.18.35.3 rule 12).
data division.
working-storage section.
01 n pic 99 value 3.
01 sn pic s99 value 3.
01 x pic x(4) value "abcd".
01 tbl.
   05 el pic x(2) occurs 2 times.
screen section.
01 s.
   05 line sn column 1 value "a".
procedure division.
    display s.
    stop run.
