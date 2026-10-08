identification division.
program-id. p-retry-std85.
*> RETRY is COBOL 2002 (14.7.9).
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
    read f retry 2 times
    close f
    goback.
