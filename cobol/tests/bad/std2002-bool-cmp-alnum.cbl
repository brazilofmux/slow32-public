identification division.
program-id. boolcmp.
*> A boolean operand is compared only with a boolean one (2023 8.8.4.2.8).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
procedure division.
    if b = "0101" display "x" end-if
    stop run.
