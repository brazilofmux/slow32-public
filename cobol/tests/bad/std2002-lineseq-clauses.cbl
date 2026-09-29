identification division.
program-id. lsclause.
*> A LINE SEQUENTIAL file takes neither RESERVE (2023 12.4.5.2 rule 12)
*> nor BLOCK or RECORD CONTAINS (13.4.5.3 rule 4): its records are lines.
environment division.
input-output section.
file-control.
    select f assign to "x.txt" organization line sequential reserve 2 areas.
data division.
file section.
fd f record contains 10 characters.
01 r pic x(10).
procedure division.
    stop run.
