identification division.
program-id. stgrp.
*> An ordinary group is not moved to a strongly-typed one (8.5.3.3, D.8.3).
data division.
working-storage section.
01  d-t typedef strong.
    05 yy pic 9999.
    05 mm pic 99.
01  m-t typedef strong.
    05 amt pic 99.
    05 cur pic xx.
01  d1 type to d-t.
01  g.
    05 x pic x(6).
procedure division.
    move g to d1
    stop run.
