identification division.
program-id. selnofc.
*> Without -dialect=mf, a SELECT with no FILE-CONTROL header is refused,
*> naming the switch (BP-D1).
environment division.
input-output section.
    select f assign to "x.txt" organization line sequential.
data division.
file section.
fd f.
01 r pic x(4).
procedure division.
    stop run.
