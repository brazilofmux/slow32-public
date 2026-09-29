identification division.
program-id. p.
data division.
working-storage section.
01 p usage pointer.
01 w pic x(4).

procedure division.
    call "x" using by reference address of w
    stop run.
