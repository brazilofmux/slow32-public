identification division.
program-id. p10.
*> assign-using: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign using k organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
01 k pic x(8) value 'x.dat'.
procedure division.
    display k
    goback.
