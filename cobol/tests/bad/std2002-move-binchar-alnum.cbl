identification division.
program-id. mv.
data division.
working-storage section.
01 b binary-char value 7.
01 x pic x(4).
procedure division.
    move b to x
    stop run.
