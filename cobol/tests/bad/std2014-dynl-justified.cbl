identification division.
program-id. p-std2014-dynl-justified.
*> JUSTIFIED on a dynamic-length item (2023 13.18.32.3 rule 4; 13.16.3 rule 18).
data division.
working-storage section.
01 s pic x dynamic length justified right.
procedure division.
    display s
    goback.
