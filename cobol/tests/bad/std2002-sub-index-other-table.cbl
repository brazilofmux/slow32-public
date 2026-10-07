identification division.
program-id. p-sub-index-other-table.
*> An index-name subscripting a table it is not an index of (2023 8.4.2.3.3
*> rule 4).
data division.
working-storage section.
01 t.
   05 e occurs 2 indexed by i1.
      10 x pic x.
   05 u occurs 2 indexed by i2.
      10 v pic 9.
procedure division.
    set i1 to 1
    display v (i1)
    goback.
