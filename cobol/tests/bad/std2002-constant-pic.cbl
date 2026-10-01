identification division.
program-id. std2002constantpic.
*> Only a positive integer constant repeats a PICTURE symbol (rule 2).
data division.
working-storage section.
01 h constant as "ab".
01 t pic x(h).
procedure division.
    stop run.
