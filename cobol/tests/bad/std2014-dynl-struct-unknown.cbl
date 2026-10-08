identification division.
program-id. p-std2014-dynl-struct-unknown.
*> A dynamic-length-structure-name not declared in SPECIAL-NAMES (2023 13.18.19.3 rule 2).
data division.
working-storage section.
01 s pic x dynamic length nosuch limit 5.
procedure division.
    display s
    goback.
