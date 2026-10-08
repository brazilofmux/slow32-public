identification division.
program-id. p-std2014-dynl-unstring-into.
*> UNSTRING INTO a dynamic-length item: not in this stage.
data division.
working-storage section.
01 s pic x dynamic length.
01 w pic x(10) value "a,b".
procedure division.
    unstring w delimited by "," into s
    goback.
