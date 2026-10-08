identification division.
program-id. sharext.
*> -warn-extensions: the SHARING and LOCK MODE clauses are 2002's (BP-E33).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization line sequential
        sharing with all other lock mode is manual.
data division.
file section.
fd f.
01 r pic x(10).
procedure division.
    stop run.
