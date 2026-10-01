identification division.
program-id. fdnosec.
*> Without -dialect=mf an FD with no FILE SECTION header is refused,
*> naming the switch (BP-D5).
environment division.
input-output section.
file-control.
    select f assign to "x.txt" organization line sequential.
data division.
fd f.
01 r pic x(4).
procedure division.
    stop run.
