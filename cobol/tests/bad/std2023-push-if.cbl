>>PUSH IF
identification division.
program-id. p-std2023-push-if.
*> PUSH names no IF directive (7.3.22.3 rule 1).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
