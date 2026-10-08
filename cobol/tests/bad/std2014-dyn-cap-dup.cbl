identification division.
program-id. p-std2014-dyn-cap-dup.
*> CAPACITY IN names an item defined elsewhere (2023 13.18.38.3 rule 30).
data division.
working-storage section.
01 c pic 9.
01 g. 05 t pic x occurs dynamic capacity in c.
procedure division.
    display c
    goback.
