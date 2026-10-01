identification division.
program-id. extasgn.
*> -warn-extensions under -dialect=mf: an implicit ASSIGN data-name (BP-D6).
environment division.
input-output section.
file-control.
    select f assign to wid-f organization line sequential.
data division.
file section.
fd f.
01 r pic x(4).
procedure division.
    stop run.
