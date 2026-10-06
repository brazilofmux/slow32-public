identification division.
program-id. p9.
*> write-before-after: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization line sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open output f
    write r before advancing 1 after advancing 2
    close f
    goback.
