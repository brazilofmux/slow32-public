identification division.
program-id. p-unlock-sortfile.
*> UNLOCK of a sort file (2023 14.9.47.3 rule 1).
environment division.
input-output section.
file-control.
    select wk assign to "wk".
data division.
file section.
sd wk.
01 wr pic x(4).
procedure division.
    unlock wk
    goback.
