identification division.
program-id. p-std2014-dynl-group-move.
*> A variable-length group holding a dynamic-length item, sent whole (2023 8.5.1.12): not in this stage.
data division.
working-storage section.
01 g. 05 s pic x dynamic length. 05 h pic x.
01 w pic x(10).
procedure division.
    move g to w
    goback.
