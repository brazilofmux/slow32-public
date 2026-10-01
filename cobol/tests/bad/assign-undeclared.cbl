identification division.
program-id. asgnimp.
*> Without -dialect=mf, an ASSIGN data-name declared nowhere is refused.
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
