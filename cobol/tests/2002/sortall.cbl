*> SORT of a table with ALL as its own subscript (2023 8.4.2.3.3 rule 6:
*> "the rightmost or only subscript of a table in the table format of a
*> SORT statement", equivalent to omitting it): a one-level table and a
*> nested one.  No oracle: GnuCOBOL 4 refuses ALL in a SORT.
*> docs/conformance/sort.md
identification division.
program-id. sortall.
data division.
working-storage section.
01 g.
   05 row occurs 2.
      10 cell occurs 3 pic 9.
01 flat.
   05 f occurs 4 pic 99.
procedure division.
    move "312" to row(1) move "978" to row(2)
    sort cell(1, all) ascending cell
    sort cell(2, all) descending cell
    display row(1) " " row(2)
    move "40102030" to flat
    sort f(all) descending f display flat
    sort f(all) ascending f display flat
    stop run.
