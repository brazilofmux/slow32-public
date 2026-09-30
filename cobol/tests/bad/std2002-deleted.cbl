identification division.
program-id. deleted.
*> COBOL 2002 deleted the elements 1985 had made obsolete (ISO/IEC
*> 1989:2002 F.1): under -std=2002 they are refused, not warned about.
environment division.
input-output section.
file-control.
    select f assign to "x.dat".
data division.
file section.
fd f label records are standard.
01 r pic x(10).
procedure division.
p1.
    go to p2.
p3.
    alter p1 to proceed to p4.
    stop "hello".
    open input f reversed.
p2.
    continue.
p4.
    stop run.
