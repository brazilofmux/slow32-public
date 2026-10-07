identification division.
program-id. p13.
*> ASSIGN USING under -std=85: the phrase is COBOL 2002's.
environment division.
input-output section.
file-control.
    select f assign using k organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
01 k pic x(8) value 'x.dat'.
procedure division.
    open input f
    goback.
