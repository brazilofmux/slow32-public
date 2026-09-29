identification division.
program-id. mv.
data division.
working-storage section.
01 v pic 9(2)v9(2) value 12.34.
01 x pic x(4).
procedure division.
    move v to x
    stop run.
