identification division.
program-id. bool85.
*> PICTURE 1 is COBOL 2002; under -std=85 it is refused.
data division.
working-storage section.
01  b pic 1(4).
procedure division.

    stop run.
