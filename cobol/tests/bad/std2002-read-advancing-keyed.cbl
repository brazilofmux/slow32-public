identification division.
program-id. p-read-advancing-keyed.
*> ADVANCING ON LOCK on a keyed READ (2023 14.9.30 format 1 alone has it).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization relative access random relative key k lock mode is manual.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
01 k pic 9(4).
procedure division.
    open i-o f
    move 1 to k
    read f advancing on lock
    close f
    goback.
