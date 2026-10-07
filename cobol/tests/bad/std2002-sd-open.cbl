identification division.
program-id. p-sd-open.
*> OPEN of a sort file: SORT, MERGE, RELEASE and RETURN name it, no other
*> statement (2023 13.4.6.3 rule 3).
environment division.
input-output section.
file-control.
    select wk assign to "wk".
data division.
file section.
sd wk.
01 wr pic x(4).
procedure division.
    open input wk
    goback.
