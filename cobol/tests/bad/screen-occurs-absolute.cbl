identification division.
program-id. occabs.
*> An OCCURS screen item with LINE and COLUMN needs PLUS or MINUS in one
*> of them, or every occurrence lands in one place (2023 13.18.38.3
*> rules 14, 15).  Accepted before.
data division.
working-storage section.
screen section.
01 s1.
   05 line 1 col 1 value 'x' occurs 3 times.
procedure division.
    display s1
    goback.
