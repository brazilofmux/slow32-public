identification division.
program-id. p11.
*> ASSIGN USING a numeric item: rule 7 wants an alphanumeric one.
environment division.
input-output section.
file-control.
    select f assign using k organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
01 k pic 9(8).
procedure division.
    open input f
    goback.
