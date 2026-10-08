identification division.
program-id. p-std2002-write-file-figurative.
*> WRITE FILE ... FROM takes no figurative constant (14.9.51.3 rule 7b).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd f record varying 1 to 20.
01 f-rec pic x(20).
working-storage section.
01 n pic 9(3) value 1.
procedure division.
    open output f.
    write file f from all "ab".
    close f.
    stop run.
