identification division.
program-id. p-std2014-constrec-linkage.
*> CONSTANT RECORD is for the WORKING-STORAGE or LOCAL-STORAGE SECTION
*> (2023 13.18.15.3 rule 1).
data division.
linkage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic 9(3) value 7.
      88 seven value 7.

01 w pic x(10).
procedure division.
    display f1
    stop run.
