identification division.
program-id. p2.
*> START of a sequential file without FIRST or LAST: it has no key to
*> start on (2023 14.9.41.3 rule 2).  With FIRST or LAST: 2002/startseq.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open input f
    start f
    close f
    goback.
