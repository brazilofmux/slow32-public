identification division.
program-id. x9699.
*> & joins literals, not data items (2023 8.8.3.1).
data division.
working-storage section.
01 x pic x.
procedure division.
    display "a" & x
    stop run.
