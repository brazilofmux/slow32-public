identification division.
program-id. p-std2002-lock-multiple-seq.
*> MULTIPLE with a sequential organization (2023 12.4.5.9.3 rule 2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential lock mode is automatic with lock on multiple records.
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
