identification division.
program-id. std2002constant.
*> A constant-name defined twice must be defined the same way
*> (2023 13.10.3 rule 9); the same value again is allowed (2002/constdup).
data division.
working-storage section.
01 k constant as 42.
01 k constant as 43.
procedure division.
    stop run.
