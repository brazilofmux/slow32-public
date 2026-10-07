identification division.
program-id. p-class-pointer.
*> A class condition on a pointer (2023 8.8.4.4.3 rule 1).
data division.
working-storage section.
01 p usage pointer.
procedure division.
    if p is numeric display "x" end-if
    goback.
