identification division.
program-id. p-type-followed-sub.
*> An entry with a TYPE clause followed by a subordinate entry (2023 13.18.57.3
*> rule 2): the type's own subordinates follow it.
data division.
working-storage section.
01 pt typedef.
   05 px pic 9(3).
01 q type pt.
   05 extra pic x.
procedure division.
    goback.
