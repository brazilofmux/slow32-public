identification division.
program-id. p-line-counter-receiver.
*> LINE-COUNTER as a receiving operand (2023 8.4.3.15.3 rule 3); PAGE-
*> COUNTER may be set.
environment division.
input-output section.
file-control.
    select prf assign to "x.txt" organization line sequential.
data division.
file section.
fd prf report is r1.
report section.
rd r1 page limit 10 lines.
01 d1 type detail.
   05 line plus 1 column 1 pic 99 source page-counter.
procedure division.
    open output prf
    initiate r1
    add 1 to line-counter
    terminate r1
    close prf
    goback.
