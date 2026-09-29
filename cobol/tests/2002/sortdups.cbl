*> SORT of a table WITH DUPLICATES (2023 14.9.40.4 general rule 3c):
*> elements whose keys are equal keep the order they had before the
*> sort -- by one key, by the table's own OCCURS KEY, by a signed key.
*> docs/conformance/sort.md
*> No oracle: GnuCOBOL's table SORT does not keep that order, with the
*> DUPLICATES phrase or without it.
identification division.
program-id. sortdups.
data division.
working-storage section.
01 t1.
   05 e1 occurs 6 ascending key k1.
      10 k1 pic x(3).
      10 v1 pic 9.
01 t2.
   05 e2 occurs 4.
      10 k2 pic s9(3).
      10 c2 pic x.
01 i pic 9.
procedure division.
    move "pea1fig2ant3fig4bee5ant6" to t1
    sort e1 ascending k1 with duplicates
    display "k1:     " t1
    move "pea1fig2ant3fig4bee5ant6" to t1
    sort e1 with duplicates in order
    display "own key:" t1
    move 12 to k2 (1) move "a" to c2 (1)
    move -5 to k2 (2) move "b" to c2 (2)
    move 7 to k2 (3) move "c" to c2 (3)
    move -5 to k2 (4) move "d" to c2 (4)
    sort e2 ascending k2 with duplicates
    perform varying i from 1 by 1 until i > 4
        display k2 (i) " " c2 (i)
    end-perform
    stop run.
