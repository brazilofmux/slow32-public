identification division.
program-id. p.
data division.
working-storage section.
01 p usage pointer.
01 w pic x(4).

procedure division.
    if p < address of w continue end-if
    stop run.
