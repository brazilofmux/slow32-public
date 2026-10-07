identification division.
program-id. p-sort-nested.
*> SORT of a table inside another (2023 8.4.2.3.3 rules 5e and 6; 14.9.40.3
*> rule 14b): the outer subscripts and no more, ALL only in the table's
*> own place, the keys unsubscripted, the table not reference-modified.
data division.
working-storage section.
01 g.
   05 row occurs 3.
      10 cell occurs 4.
         15 cv pic 99.
         15 cn pic x.
01 i pic 9.
procedure division.
    sort cell ascending cv.
    sort cell(1, 2) ascending cv.
    sort cell(all) ascending cv.
    sort cell(1) ascending cv(1).
    sort cell(i)(1:2) ascending cv.
    stop run.
