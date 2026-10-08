identification division.
program-id. p-std2014-dyn-nested.
*> A dynamic-capacity table inside a table: allowed by 2023 8.5.1.9.1, not in this stage.
data division.
working-storage section.
01 g. 05 r occurs 2. 10 t pic x occurs dynamic capacity in c.
procedure division.
    move "a" to t(1, 1)
    goback.
