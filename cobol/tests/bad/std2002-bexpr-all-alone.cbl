identification division.
program-id. bexall1.
*> A boolean COMPUTE is not an ALL literal alone (2023 14.9.8.3 rule 3).
data division.
working-storage section.
01  b pic 1(4).
procedure division.
    compute b = all b"10"
    stop run.
