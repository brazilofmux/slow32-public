identification division.
program-id. p-apply-commit.
*> APPLY COMMIT in I-O-CONTROL: COBOL 2023, not implemented.
environment division.
input-output section.
file-control.
    select a1 assign to "x1" organization line sequential.
i-o-control.
    apply commit on a1.
data division.
file section.
fd a1.
01 r1 pic x(4).
procedure division.
    goback.
