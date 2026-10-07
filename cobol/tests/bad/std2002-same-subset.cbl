identification division.
program-id. p-same-subset.
*> A SAME AREA set overlapping a SAME RECORD AREA set without lying within it
*> (2023 12.4.6.4.3 rule 9).
environment division.
input-output section.
file-control.
    select a1 assign to "x1" organization line sequential.
    select a2 assign to "x2" organization line sequential.
    select a3 assign to "x3" organization line sequential.
    select wk assign to "wk".
i-o-control.
    same record area for a1 a2.
    same area for a1 a3.
data division.
file section.
fd a1.
01 r1 pic x(4).
fd a2.
01 r2 pic x(4).
fd a3.
01 r3 pic x(4).
sd wk.
01 wr pic x(4).
procedure division.
    goback.
