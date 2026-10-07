identification division.
program-id. p-numed-value-2014.
*> A numeric VALUE for a numeric-edited item is COBOL 2023 (13.18.63.3 rule 6; E.3.3 item 43).
data division.
working-storage section.
01 x pic zz9 value 12.
procedure division.
    display x
    stop run.
