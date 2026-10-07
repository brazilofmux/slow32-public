identification division.
program-id. sub1 is prototype.
*> A program prototype with an INPUT-OUTPUT SECTION (2023 10.6.2 rule 4d).
environment division.
input-output section.
file-control.
    select f assign to "x" organization line sequential.
data division.
file section.
fd f.
01 r pic x.
procedure division.
end program sub1.
