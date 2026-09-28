identification division.
program-id. boolspc.
*> SPACE is no boolean value (2023 14.9.25 rule 7).
data division.
working-storage section.
01  b pic 1(4).
procedure division.
    move space to b
    stop run.
