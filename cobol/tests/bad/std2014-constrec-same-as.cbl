identification division.
program-id. p-std2014-constrec-same-as.
*> SAME AS a structured constant (2023 13.18.49.3 rule 10).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.
01 s same as c.
01 w pic x(10).
procedure division.
    display s
    stop run.
