*> SORT of a table inside another table (2023 14.9.40 format 2, 8.4.2.3.3
*> rules 5e and 6): the table is written with the subscripts of the tables
*> it is inside and its own omitted, or ALL in its place; the one table
*> they select is sorted, the others untouched.  Two and three levels, a
*> literal, an item and an index as the outer subscript, the table's own
*> OCCURS KEY, DESCENDING.  The ALL form is in sortall (GnuCOBOL refuses
*> it).  GnuCOBOL agrees.
*> docs/conformance/sort.md
identification division.
program-id. sortnested.
data division.
working-storage section.
01 g.
   05 row occurs 3 indexed by rx.
      10 cell occurs 4 ascending key cv.
         15 cv pic 99.
         15 cn pic x.
01 g3.
   05 a occurs 2.
      10 b occurs 2.
         15 c occurs 3.
            20 ck pic 9.
            20 cx pic x.
01 i pic 9.
01 flat.
   05 f occurs 4 pic 99.
procedure division.
    move "31a12b45c07d" to row(1)
    move "99w05x50y50z" to row(2)
    move "01q02r03s04t" to row(3)
    sort cell(1)
    display row(1) " " row(2) " " row(3)
    sort cell(2) on descending key cv
    display row(1) " " row(2) " " row(3)
    sort cell(3) descending key cn
    display row(1) " " row(2) " " row(3)
    set rx to 1 sort cell(rx) descending cv
    move 2 to i sort cell(i) ascending cn
    display row(1) " " row(2) " " row(3)
    move "3c1a2b" to b(1, 1) move "9z8y7x" to b(1, 2) move "5m4n6o" to b(2, 1) move "1z1y1x" to b(2, 2)
    sort c(1, 1) ascending ck
    display a(1) " " a(2)
    sort c(1, 2) ascending ck
    sort c(2, 1) descending ck
    sort c(2, 2) ascending cx
    display a(1) " " a(2)
    move "40102030" to flat
    sort f ascending f display flat
    stop run.
