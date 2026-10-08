identification division.
program-id. p-std2002-rewrite-lock-automatic.
*> REWRITE WITH NO LOCK under automatic locking (2023 14.9.35.3 rule 4).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic record key r lock mode is automatic.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open i-o f
    read f rewrite r with no lock
    close f
    goback.
