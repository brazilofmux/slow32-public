identification division.
program-id. p-constrec-redefines.
*> REDEFINES and CONSTANT RECORD are not in one entry (2023 13.16.3 rule 3).
data division.
working-storage section.
01 a pic x(8).
01 c redefines a constant record.
   02 f1 pic x(5) value "abcde".
procedure division.
    display f1
    stop run.
