identification division.
program-id. stcmp.
*> A strongly-typed group compares only with one of its type (8.8.4.2.12).
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
    if d1 = m1 display "x" end-if
    stop run.
