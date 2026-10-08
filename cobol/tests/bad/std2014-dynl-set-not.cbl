identification division.
program-id. p-std2014-dynl-set-not.
*> SET SIZE OF an item that is not dynamic-length (2023 14.9.39.3 rule 33).
data division.
working-storage section.
01 s pic x(5).
procedure division.
    set size of s to 3
    goback.
