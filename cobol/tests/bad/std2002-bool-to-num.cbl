identification division.
program-id. boolton.
*> A boolean item is not moved to a numeric one (2023 14.9.25 table).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
01  k pic 99.
procedure division.
    move b to k
    stop run.
