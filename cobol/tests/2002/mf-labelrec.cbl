*> LABEL RECORDS and DATA RECORDS under -std=2002 with -dialect=mf:
*> COBOL 2002 deleted both, Micro Focus keeps them as documentary only
*> (its clauses' rule 1), so a 2002 program in MF's dialect carries them
*> and they do nothing.  ACAS (cobol ISSUES-124) needs 2002 for its
*> eight-byte COMP-X and writes LABEL RECORDS in its FDs.
identification division.
program-id. mf-labelrec.
environment division.
input-output section.
file-control.
    select f1 assign to "LABELREC.DAT"
        organization line sequential.
data division.
file section.
fd  f1
    label records are standard
    data record is r1.
01  r1  pic x(10).
working-storage section.
01  w   pic x(10).
procedure division.
    open output f1
    move "LABELLED" to r1
    write r1
    close f1
    open input f1
    read f1 into w
    close f1
    display w
    goback.
