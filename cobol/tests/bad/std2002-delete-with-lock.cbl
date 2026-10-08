identification division.
program-id. p-std2002-delete-with-lock.
*> DELETE takes RETRY alone, no LOCK phrase (2023 14.9.10.2).
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
    read f delete f record with lock
    close f
    goback.
