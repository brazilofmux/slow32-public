identification division.
program-id. p5.
*> START FIRST under -std=85: the phrase is COBOL 2002's (14.9.41).
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
