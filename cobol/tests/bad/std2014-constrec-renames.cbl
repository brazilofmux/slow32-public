identification division.
program-id. p-std2014-constrec-renames.
*> RENAMES of entries in a structured constant (2023 13.18.45.3 rule 6).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.
66 r renames f1 thru f2.
01 w pic x(10).
procedure division.
    display r
    stop run.
