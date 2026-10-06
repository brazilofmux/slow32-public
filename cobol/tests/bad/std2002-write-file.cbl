identification division.
program-id. p6.
*> write-file: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open output f
    write file f from r
    close f
    goback.
