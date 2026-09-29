identification division.
program-id. p.
data division.
working-storage section.
01 p usage pointer.
01 w pic x(4).
01 b pic x(4) based.

procedure division.
    allocate w
    stop run.
