identification division.
program-id. bexshf.
*> A boolean shift takes an integer count (2023 8.8.2 rule 5).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
procedure division.
    compute b = b b-shift-l b"1"
    stop run.
