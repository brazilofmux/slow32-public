identification division.
program-id. p-std2014-zero-unstring.
*> An UNSTRING delimiter is not a zero-length literal (2023 14.9.48.3 rule 1).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.
01 r pic x(8).
procedure division.
    unstring x delimited by "" into r
    stop run.
