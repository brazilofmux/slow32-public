identification division.
program-id. p-type-under-usage.
*> A TYPE entry under a group with a USAGE clause (2023 13.18.57.3 rule 5).
data division.
working-storage section.
01 pt typedef pic x(2).
01 g sign leading separate.
   05 h type pt.
procedure division.
    goback.
