identification division.
program-id. p-std2014-dynl-accept.
*> ACCEPT into a dynamic-length item: not in this stage.
data division.
working-storage section.
01 s pic x dynamic length.
procedure division.
    accept s
    goback.
