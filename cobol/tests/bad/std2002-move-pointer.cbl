identification division.
program-id. mv.
data division.
working-storage section.
01 p usage pointer.
01 x pic x(4).
procedure division.
    move p to x
    stop run.
