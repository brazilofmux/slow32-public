identification division.
program-id. stlvl.
*> A strong type is used at level 01 or inside a strong type (13.18.57.3 rule 6).
data division.
working-storage section.
01  d-t typedef strong.
    05 yy pic 9999.
    05 mm pic 99.
01  m-t typedef strong.
    05 amt pic 99.
    05 cur pic xx.
01  g.
    05 d1 type to d-t.
procedure division.

    stop run.
