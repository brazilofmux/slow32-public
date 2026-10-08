identification division.
program-id. p-std2014-dyn-set-below-from.
*> SET capacity TO below the minimum (2023 14.9.39.3 rule 30).
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic capacity in c from 2 to 5.
procedure division.
    set c to 1
    goback.
