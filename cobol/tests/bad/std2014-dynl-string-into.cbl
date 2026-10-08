identification division.
program-id. p-std2014-dynl-string-into.
*> STRING INTO a dynamic-length item: not in this stage.
data division.
working-storage section.
01 s pic x dynamic length.
procedure division.
    string "a" "b" delimited by size into s
    goback.
