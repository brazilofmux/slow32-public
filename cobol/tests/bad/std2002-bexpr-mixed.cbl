identification division.
program-id. bexmix.
*> A COMPUTE stores a boolean or a number, not both (2023 14.9.8.3).
data division.
working-storage section.
01  b pic 1(4).
01  k pic 9.
procedure division.
    compute b k = b"1"
    stop run.
