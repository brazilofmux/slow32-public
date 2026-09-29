identification division.
program-id. p.
data division.
working-storage section.
01 p usage pointer.
01 w pic x(4).

procedure division.
    if p = 5 continue end-if
    stop run.
