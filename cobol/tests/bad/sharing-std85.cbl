identification division.
program-id. p-sharing-std85.
*> OPEN's SHARING phrase is COBOL 2002 (14.9.27); the clause itself is BP-E33.
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
    open i-o sharing with all other f
    close f
    goback.
