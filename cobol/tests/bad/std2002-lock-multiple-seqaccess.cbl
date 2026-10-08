identification division.
program-id. p-std2002-lock-multiple-seqaccess.
*> MULTIPLE with sequential access (2023 12.4.5.9.3 rule 2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access sequential record key r lock mode is manual with lock on multiple records.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open i-o f
    read f
    close f
    goback.
