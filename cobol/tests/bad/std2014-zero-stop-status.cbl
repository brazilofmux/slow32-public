identification division.
program-id. p-std2014-zero-stop-status.
*> STOP RUN WITH ... STATUS literal: not a zero-length literal (2023 14.9.42.3 rule 4).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    stop run with error status ""
    stop run.
