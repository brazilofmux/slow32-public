identification division.
program-id. p.
data division.
working-storage section.
01 n pic 9(4).
01 q pic 99.
01 r pic 99.
procedure division.
    divide n by 2 giving q r remainder r
    stop run.
