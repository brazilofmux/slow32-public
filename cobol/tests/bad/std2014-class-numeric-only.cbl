identification division.
program-id. p-class-numeric-only.
*> FARTHEST-FROM-ZERO of an alphanumeric item: a numeric one (2023 8.8.4.4.3
*> rule 6).
data division.
working-storage section.
01 x pic x(3).
procedure division.
    if x is farthest-from-zero display "x" end-if
    goback.
