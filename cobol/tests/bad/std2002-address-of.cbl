identification division.
program-id. p.
data division.
working-storage section.
01 p usage pointer.
01 w pic x.
procedure division.
    set p to address of w
    stop run.
