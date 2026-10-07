identification division.
program-id. p-constrec-level.
*> CONSTANT RECORD is a level 01 clause (2023 13.16.3 rule 6).
data division.
working-storage section.
01 c.
   02 f1 pic x(5) constant record value "abcde".
procedure division.
    display f1
    stop run.
