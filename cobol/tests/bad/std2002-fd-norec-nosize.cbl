identification division.
program-id. p.
*> An FD with no record description entry needs a RECORD clause (2023 13.4.5.3 rule 3a).
environment division.
input-output section.
file-control.
    select f assign to "x.dat".
data division.
file section.
fd  f.
procedure division.
    stop run.
end program p.
