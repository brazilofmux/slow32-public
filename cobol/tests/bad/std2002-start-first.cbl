identification division.
program-id. p0.
*> start-first: refused by name (docs/plans/standard-queue.md item 1); it was a
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
    open input f
    start f first
    close f
    goback.
