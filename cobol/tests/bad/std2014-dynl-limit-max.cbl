identification division.
program-id. p-std2014-dynl-limit-max.
*> LIMIT above the implementor's maximum (2023 13.18.19.4 rule 2).
data division.
working-storage section.
01 s pic x dynamic length limit 99999999.
procedure division.
    display s
    goback.
