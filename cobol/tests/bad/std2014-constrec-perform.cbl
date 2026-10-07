identification division.
program-id. p-std2014-constrec-perform.
*> PERFORM VARYING an item of a structured constant (2023 13.18.15.3 rule 2).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.

01 w pic x(10).
procedure division.
    perform varying f2 from 1 by 1 until f2 > 3 display f2 end-perform
    stop run.
