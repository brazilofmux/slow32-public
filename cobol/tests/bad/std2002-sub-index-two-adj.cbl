identification division.
program-id. p-sub-index-two-adj.
*> An index-name subscript with two integers added: the form is one
*> integer, added or subtracted (2023 8.4.2.3).
data division.
working-storage section.
01 t.
   05 v pic 9 occurs 3 indexed by ix.
procedure division.
    set ix to 1
    display v (ix + 1 + 1)
    goback.
