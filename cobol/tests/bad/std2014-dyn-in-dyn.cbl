identification division.
program-id. p-std2014-dyn-in-dyn.
*> A dynamic-capacity table inside a dynamic-capacity table: allowed by 2023 8.5.1.9.1, not in this stage.
data division.
working-storage section.
01 g. 05 t occurs dynamic. 10 u pic x occurs dynamic capacity in c.
procedure division.
    move "a" to u(1, 1)
    goback.
