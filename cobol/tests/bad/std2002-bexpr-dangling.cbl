identification division.
program-id. bexdng.
*> A boolean expression ends with an operand (2023 8.8.2 rule 2).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
procedure division.
    compute b = b b-and
    stop run.
