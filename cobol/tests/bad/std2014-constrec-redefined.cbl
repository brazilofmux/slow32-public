identification division.
program-id. p-std2014-constrec-redefined.
*> An entry REDEFINES a structured constant (2023 13.18.44.3 rule 13).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.
01 d redefines c pic x(8).
01 w pic x(10).
procedure division.
    display d
    stop run.
