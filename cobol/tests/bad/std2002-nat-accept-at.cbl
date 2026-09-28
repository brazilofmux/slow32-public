identification division.
program-id. nataat.
*> ACCEPT of a national item at a screen position waits for national
*> screen fields; until then it is refused.
data division.
working-storage section.
01  n pic n(4).
procedure division.
    accept n at line 1 column 1
    stop run.
