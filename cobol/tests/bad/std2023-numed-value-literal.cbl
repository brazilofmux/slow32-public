identification division.
program-id. p-numed-value-literal.
*> An alphanumeric VALUE of a numeric-edited item is its picture edited (2023 13.18.63.3 rule 7; E.2 items 27, 29).
data division.
working-storage section.
01 x pic zz9.99 value "012.50".
procedure division.
    display x
    stop run.
