identification division.
program-id. p-class-alpha-numeric.
*> ALPHABETIC of a numeric item (2023 8.8.4.4.3 rule 4).
data division.
working-storage section.
01 n pic 9(3) value 12.
procedure division.
    if n is alphabetic display "x" end-if
    goback.
