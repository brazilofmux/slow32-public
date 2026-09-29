identification division.
program-id. bexall2.
*> The operands of a boolean operation are not both ALL literals (2023 8.8.2 rule 4).
data division.
working-storage section.
01  b pic 1(4).
procedure division.
    compute b = all b"1" b-and all b"10"
    stop run.
