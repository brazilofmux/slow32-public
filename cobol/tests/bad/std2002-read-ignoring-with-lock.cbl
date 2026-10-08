identification division.
program-id. p-std2002-read-ignoring-with-lock.
*> IGNORING LOCK beside a LOCK phrase (2023 14.9.30.3 rule 3).
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
    read f with lock ignoring lock
    close f
    goback.
