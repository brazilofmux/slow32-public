identification division.
program-id. p-std2014-dyn-group-recv.
*> A variable-length group received whole (2023 8.5.1.12): not in this stage.
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic capacity in c. 05 h pic x.
01 w pic x(10).
procedure division.
    move w to g
    goback.
