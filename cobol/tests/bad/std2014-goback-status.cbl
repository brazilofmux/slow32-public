identification division.
program-id. p-std2014-goback-status.
*> GOBACK WITH ... STATUS is COBOL 2023 (14.9.18).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    goback with error status 3
    stop run.
