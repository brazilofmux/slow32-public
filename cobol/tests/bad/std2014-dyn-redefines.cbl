identification division.
program-id. p-std2014-dyn-redefines.
*> REDEFINES of a dynamic-capacity table (2023 13.18.44.3 rule 17).
data division.
working-storage section.
01 g. 05 h. 10 t pic x(4) occurs dynamic capacity in c. 05 r redefines h pic x(8).
procedure division.
    display c
    goback.
