>>PUSH DEFINE
identification division.
program-id. p-std2014-push.
*> PUSH is COBOL 2023 (7.3.22).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
