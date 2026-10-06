identification division.
program-id. p29.
*> CONSTANT ... FROM names a compilation variable a >>DEFINE made (2023
*> 13.10); a data item is not one.
data division.
working-storage section.
01 a pic x.
01 k constant from a.
procedure division.
    display 'x'
    goback.
