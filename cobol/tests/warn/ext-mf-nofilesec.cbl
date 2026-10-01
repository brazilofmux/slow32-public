identification division.
program-id. extfdns.
*> -warn-extensions under -dialect=mf: no FILE SECTION header (BP-D5).
environment division.
input-output section.
file-control.
    select f assign to "x.txt" organization line sequential.
data division.
fd f.
01 r pic x(4).
procedure division.
    stop run.
