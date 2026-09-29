identification division.
program-id. b88.
*> THROUGH is not specified in a condition-name's VALUE for a boolean
*> conditional variable (2023 13.18.63.3 rule 29; cobol ISSUES-94 B12).
data division.
working-storage section.
01  m pic 1(2).
    88 m-rng value b"01" thru b"11".
procedure division.
    if m-rng display "in" end-if
    stop run.
