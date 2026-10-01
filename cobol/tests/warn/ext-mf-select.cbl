identification division.
program-id. extmfsel.
*> -warn-extensions under -dialect=mf: BP-D1 (and free form, BP-E11).
environment division.
input-output section.
    select f assign to "x.txt" organization line sequential.
data division.
file section.
fd f.
01 r pic x(4).
procedure division.
    stop run.
