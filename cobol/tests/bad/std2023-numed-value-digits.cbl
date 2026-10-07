identification division.
program-id. p-numed-value-digits.
*> A numeric VALUE for a numeric-edited item: no truncation of digits (2023 13.18.63.3 rule 6).
data division.
working-storage section.
01 x pic zz9 value 1234.
procedure division.
    display x
    stop run.
