identification division.
program-id. ecpfn.
*> WHEN EXCEPTION with a bare file-name comes later; refused by name.
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
    end-perform
    stop run.
