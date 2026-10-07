identification division.
program-id. p-std2014-zero-cancel.
*> CANCEL names a program with a literal that is not zero-length (2023 14.9.5.3 rule 2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    cancel ""
    stop run.
