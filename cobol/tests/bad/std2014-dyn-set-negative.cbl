identification division.
program-id. p-std2014-dyn-set-negative.
*> SET capacity TO a negative integer (2023 14.9.39.3 rule 30).
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic capacity in c.
procedure division.
    set c to -1
    goback.
