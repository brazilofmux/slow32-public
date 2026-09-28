identification division.
program-id. gu85.
*> GROUP-USAGE is COBOL 2002; under -std=85 it is refused.
data division.
working-storage section.
01  g group-usage national.
    05 a pic x(2).
procedure division.
    stop run.
