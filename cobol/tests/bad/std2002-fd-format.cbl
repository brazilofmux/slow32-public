identification division.
program-id. p12.
*> fd-format: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd f format is variable.
01 r pic x(10).
working-storage section.
procedure division.
    display 'x'
    goback.
