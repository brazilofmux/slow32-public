identification division.
program-id. p-std2014-dyn-init-replacing.
*> INITIALIZE REPLACING over a group holding a dynamic-capacity table: not in this stage.
data division.
working-storage section.
01 g. 05 t pic 9 occurs dynamic capacity in c. 05 h pic 9.
procedure division.
    initialize g replacing numeric by 5
    goback.
