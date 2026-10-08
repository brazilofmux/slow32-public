identification division.
program-id. p-std2002-read-lock-nolock.
*> WITH LOCK and WITH NO LOCK in one READ (2023 14.9.30.2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic record key r lock mode is manual.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open i-o f
    read f with lock with no lock
    close f
    goback.
