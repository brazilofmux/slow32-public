identification division.
program-id. p-std2014-dyn-from-to.
*> OCCURS DYNAMIC FROM not less than TO (2023 13.18.38.3 rule 28).
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic from 5 to 5.
procedure division.
    display "x"
    goback.
