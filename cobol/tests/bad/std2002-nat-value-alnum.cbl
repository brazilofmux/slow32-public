identification division.
program-id. natval.
*> A national item's VALUE is a national literal (2023 13.18.63 syntax
*> rule 5), not an alphanumeric one.
data division.
working-storage section.
01  n pic n(4) value "abc".
procedure division.
    stop run.
