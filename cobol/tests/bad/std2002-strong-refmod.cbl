identification division.
program-id. strrm.
*> A strongly-typed group is not reference-modified (8.4.2.4).
data division.
working-storage section.
01  d-t typedef strong.
    05 yy pic 9999.
    05 mm pic 99.
01  m-t typedef strong.
    05 amt pic 99.
    05 cur pic xx.
01  d1 type to d-t.
procedure division.
    display d1(1:2)
    stop run.
