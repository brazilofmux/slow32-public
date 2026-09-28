identification division.
program-id. bexnum.
*> A boolean expression takes boolean operands (2023 8.8.2).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
01  k pic 9 value 1.
procedure division.
    compute b = b b-and k
    stop run.
