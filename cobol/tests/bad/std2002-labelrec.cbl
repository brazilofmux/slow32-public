identification division.
program-id. labelrecbad.
*> LABEL RECORDS is deleted in COBOL 2002: refused under -std=2002
*> without -dialect=mf, the message naming both ways out (cobol
*> ISSUES-124).
environment division.
input-output section.
file-control.
    select f1 assign to "X.DAT" organization line sequential.
data division.
file section.
fd  f1 label records are standard.
01  r1  pic x(10).
procedure division.
    goback.
