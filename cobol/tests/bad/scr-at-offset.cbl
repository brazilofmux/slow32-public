identification division.
program-id. scratoff.
*> A screen placed AT another origin than line 1, column 1 is not
*> implemented, and says so (cobol ISSUES-124).
data division.
screen section.
01  s1.
    03  value "X" line 1 col 1.
procedure division.
    display s1 at 0510
    stop run.
