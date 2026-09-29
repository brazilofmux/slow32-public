identification division.
program-id. ecpf.
*> FILE follows only an EC-I-O exception-name (2023 14.9.28.3 rule 16).
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
    when exception ec-size file f
        continue
    end-perform
    stop run.
