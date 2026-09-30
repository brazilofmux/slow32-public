identification division.
program-id. p.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization line sequential.
data division.
file section.
fd f.
01 r pic x.
procedure division.
declaratives.
dg section.
    use global after error procedure on f.
d1.
    goback.
end declaratives.
main section.
m1.
    stop run.
