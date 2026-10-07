identification division.
program-id. p3.
*> START FIRST of a line sequential file: its records have no fixed
*> place; 2023 14.9.41 is for sequential, relative and indexed files.
environment division.
input-output section.
file-control.
    select f assign to "x.txt" organization line sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open input f
    start f first
    close f
    goback.
