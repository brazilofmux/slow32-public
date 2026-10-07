identification division.
program-id. p-constrec-external.
*> CONSTANT RECORD with EXTERNAL needs a strongly typed TYPE (2023 13.16.3 rule 13); not implemented.
data division.
working-storage section.
01 c external constant record.
   02 f1 pic x(5) value "abcde".
procedure division.
    display f1
    stop run.
