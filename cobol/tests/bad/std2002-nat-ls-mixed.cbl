identification division.
program-id. natlsmix.
*> A line sequential file of national records is UTF-8 text, of
*> alphanumeric records bytes; one file is one or the other.
environment division.
input-output section.
file-control.
    select f assign to "x.txt" organization line sequential.
data division.
file section.
fd  f.
01  r1 pic n(4).
01  r2 pic x(8).
procedure division.
    stop run.
