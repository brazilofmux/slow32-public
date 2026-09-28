identification division.
program-id. boolwide.
*> A boolean condition is one boolean position (2023 8.8.4.3.3 rule 1).
data division.
working-storage section.
01  b pic 1(4) value b"0101".
procedure division.
    if b display "x" end-if
    stop run.
