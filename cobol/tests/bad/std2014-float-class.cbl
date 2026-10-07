identification division.
program-id. p-float-class.
*> ALPHABETIC of a standard floating-point item: NUMERIC is its one class
*> condition (2023 8.8.4.4.3 rule 3).
data division.
working-storage section.
01 f usage float-binary-128.
procedure division.
    if f is alphabetic display "x" end-if
    goback.
