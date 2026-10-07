identification division.
program-id. p-std2014-continue-after.
*> CONTINUE AFTER ... SECONDS is COBOL 2023 (14.9.9).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    continue after 1 seconds
    stop run.
