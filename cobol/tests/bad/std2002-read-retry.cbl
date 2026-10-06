identification division.
program-id. p4.
*> read-retry: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic record key r.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open i-o f
    read f retry 3 times
    close f
    goback.
