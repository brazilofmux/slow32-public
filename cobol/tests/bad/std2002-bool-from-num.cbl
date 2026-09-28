identification division.
program-id. boolnum.
*> A numeric item is not moved to a boolean one (2023 14.9.25 table).
data division.
working-storage section.
01  b pic 1(4).
01  k pic 99 value 5.
procedure division.
    move k to b
    stop run.
