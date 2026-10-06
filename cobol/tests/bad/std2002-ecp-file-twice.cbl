identification division.
program-id. ecpft.
*> A file-name stands alone in one WHEN phrase only (2023 14.9.28.3 rule
*> 14).
environment division.
input-output section.
file-control.
    select f assign to "x.dat".
data division.
file section.
fd  f.
01  r pic x.
procedure division.
    perform
        continue
    when exception f
        continue
    when exception f
        continue
    end-perform
    stop run.
