identification division.
program-id. p-class-float-only.
*> FLOAT-NOT-A-NUMBER of a DISPLAY item: the floating-point class conditions
*> test a floating-point item (2023 8.8.4.4.3 rule 7).
data division.
working-storage section.
01 p pic 9(3).
procedure division.
    if p is float-not-a-number display "x" end-if
    goback.
