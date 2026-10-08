identification division.
program-id. p-std2014-dynl-constrec.
*> DYNAMIC LENGTH in a CONSTANT RECORD (2023 13.16.3 rule 13).
data division.
working-storage section.
01 g constant record. 05 s pic x dynamic length.
procedure division.
    display s
    goback.
