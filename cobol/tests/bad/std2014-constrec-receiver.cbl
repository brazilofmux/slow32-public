identification division.
program-id. p-std2014-constrec-receiver.
*> No statement names a structured constant or an item in it as a
*> receiving operand (2023 13.18.15.3 rule 2).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.

01 w pic x(10).
procedure division.
    move w to f1
    stop run.
