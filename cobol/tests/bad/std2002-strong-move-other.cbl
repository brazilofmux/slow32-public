identification division.
program-id. stmv.
*> A strongly-typed group receives only a group of its own type (14.9.25.3 rule 2).
data division.
working-storage section.
01  d-t typedef strong.
    05 yy pic 9999.
    05 mm pic 99.
01  m-t typedef strong.
    05 amt pic 99.
    05 cur pic xx.
01  d1 type to d-t.
01  m1 type to m-t.
procedure division.
    move m1 to d1
    stop run.
