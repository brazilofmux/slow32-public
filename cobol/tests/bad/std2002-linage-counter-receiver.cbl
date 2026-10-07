identification division.
program-id. p-linage-counter-receiver.
*> LINAGE-COUNTER as a receiving operand (2023 8.4.3.14.3 rule 2).
environment division.
input-output section.
file-control.
    select prf assign to "x.txt" organization line sequential.
data division.
file section.
fd prf linage 10 lines.
01 pl pic x(10).
working-storage section.
procedure division.
    open output prf
    move 5 to linage-counter
    close prf
    goback.
