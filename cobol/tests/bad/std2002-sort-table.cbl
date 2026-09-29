identification division.
program-id. sorttbl.
*> SORT of a table (2023 14.9.40.3 rules 13-15): a table entry, its keys
*> the entry or inside it, not in a nested table, not boolean, and a KEY
*> phrase unless the OCCURS clause has one.
data division.
working-storage section.
01 w pic x(4).
01 tb.
   05 te occurs 5.
      10 tk pic x(2).
      10 tb1 pic 1 usage bit.
      10 to2 pic x occurs 2.
procedure division.
    sort tb ascending tk.
    sort te ascending w.
    sort te ascending to2.
    sort te ascending tb1.
    sort te.
    stop run.
