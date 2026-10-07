identification division.
program-id. p-std2014-zero-national-of.
*> The argument of NATIONAL-OF is not a zero-length literal (2023 15.66.3 rule 3).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    display function national-of("")
    stop run.
