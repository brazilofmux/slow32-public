identification division.
program-id. p12.
*> ASSIGN USING an item of the file's own record (2023 12.4.5.2 rule 7).
environment division.
input-output section.
file-control.
    select f assign using k organization sequential.
data division.
file section.
fd f.
01 r.
   05 k pic x(10).
working-storage section.
procedure division.
    open input f
    goback.
