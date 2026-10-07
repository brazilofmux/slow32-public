identification division.
program-id. p-std2014-zero-delimiter.
*> A STRING delimiter is not a zero-length literal (2023 14.9.43.3 rule 3).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.
01 r pic x(8).
procedure division.
    string x delimited by "" into r
    stop run.
