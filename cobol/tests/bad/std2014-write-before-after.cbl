identification division.
program-id. p-std2014-write-before-after.
*> WRITE with both BEFORE and AFTER ADVANCING is COBOL 2023 (14.9.51).
environment division.
input-output section.
file-control.
    select f assign to "p.dat" organization sequential.
data division.
file section.
fd f.
01 frec pic x(10).
procedure division.
    open output f write frec after 1 before 1 close f
    stop run.
