identification division.
program-id. std2002constantexpr.
*> A constant entry's expression gives an integer (2023 13.10.4 rule 4).
data division.
working-storage section.
01 half constant as 7 / 2.
procedure division.
    stop run.
