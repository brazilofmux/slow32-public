identification division.
program-id. p1.
*> start-length: refused by name (docs/plans/standard-queue.md item 1); it was a
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
01 k pic 9 value 3.
procedure division.
    open input f
    start f key = r with length k
    close f
    goback.
