identification division.
program-id. p10.
*> ASSIGN USING a data-name declared nowhere: the standard's form names
*> an alphanumeric data item (2023 12.4.5.2 rule 7); the implicit
*> declaration is Micro Focus's, for ASSIGN TO (BP-D6, -dialect=mf).
environment division.
input-output section.
file-control.
    select f assign using k organization sequential.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
procedure division.
    open input f
    goback.
