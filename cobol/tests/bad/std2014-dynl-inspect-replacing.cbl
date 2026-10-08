identification division.
program-id. p-std2014-dynl-inspect-replacing.
*> INSPECT REPLACING of a dynamic-length item: not in this stage.
data division.
working-storage section.
01 s pic x dynamic length value "abc".
procedure division.
    inspect s replacing all "a" by "b"
    goback.
