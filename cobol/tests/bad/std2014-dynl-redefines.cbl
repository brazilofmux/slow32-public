identification division.
program-id. p-std2014-dynl-redefines.
*> REDEFINES of a dynamic-length item (2023 13.18.44.3 rule 12).
data division.
working-storage section.
01 g. 05 s pic x dynamic length. 05 r redefines s pic x(8).
procedure division.
    display r
    goback.
