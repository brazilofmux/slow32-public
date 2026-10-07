identification division.
program-id. p-std2023-write-both-page.
*> BEFORE and AFTER together, not with PAGE (2023 14.9.51.3 rule 17).
environment division.
input-output section.
file-control.
    select f assign to "p.dat" organization sequential.
data division.
file section.
fd f.
01 frec pic x(10).
procedure division.
    open output f write frec after page before 1 close f
    stop run.
