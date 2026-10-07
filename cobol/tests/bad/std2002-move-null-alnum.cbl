identification division.
program-id. p-move-null-alnum.
*> MOVE NULL to an alphanumeric item: NULL goes with the pointer classes
*> (2023 8.4.3.10.3 rule 1).
data division.
working-storage section.
01 x pic x(5).
procedure division.
    move null to x
    goback.
