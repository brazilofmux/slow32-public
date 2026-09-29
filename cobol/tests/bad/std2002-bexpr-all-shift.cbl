identification division.
program-id. bexalls.
*> A shift does not shift an ALL literal (2023 8.8.2 rule 5).
data division.
working-storage section.
01  b pic 1(4).
procedure division.
    compute b = all b"10" b-shift-l 1
    stop run.
