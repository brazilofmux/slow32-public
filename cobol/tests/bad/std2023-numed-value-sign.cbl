identification division.
program-id. p-numed-value-sign.
*> A numeric VALUE for a numeric-edited item: no truncation of the sign (2023 13.18.63.3 rule 6).
data division.
working-storage section.
01 x pic zz9 value -1.
procedure division.
    display x
    stop run.
