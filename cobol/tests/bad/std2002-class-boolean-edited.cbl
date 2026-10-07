identification division.
program-id. p-class-boolean-edited.
*> BOOLEAN of a numeric-edited item (2023 8.8.4.4.3 rule 5).
data division.
working-storage section.
01 ne pic zz9 value " 12".
procedure division.
    if ne is boolean display "x" end-if
    goback.
