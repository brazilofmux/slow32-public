identification division.
program-id. p11.
*> suppress-when: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic record key r1 alternate record key r2 suppress when space.
data division.
file section.
fd f.
01 r. 05 r1 pic x(5). 05 r2 pic x(5).
working-storage section.
procedure division.
    display 'x'
    goback.
