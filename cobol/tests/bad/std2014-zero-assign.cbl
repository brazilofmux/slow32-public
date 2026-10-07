identification division.
program-id. p-std2014-zero-assign.
*> ASSIGN TO literal: not a zero-length literal (2023 12.4.5.2 rule 4).
environment division.
input-output section.
file-control.
    select f assign to "" organization line sequential.
data division.
file section.
fd f.
01 frec pic x(10).
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    display x
    stop run.
