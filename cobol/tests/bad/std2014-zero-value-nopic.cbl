identification division.
program-id. p-std2014-zero-value-nopic.
*> A VALUE literal implies a PICTURE only when it is not zero-length
*> (2023 13.16.3 rule 9); without one the entry has no PICTURE (rule 8).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.
01 v value "".
procedure division.
    display v
    stop run.
