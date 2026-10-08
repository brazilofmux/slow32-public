identification division.
program-id. p-std2014-dynl-set-negative.
*> SET SIZE OF TO a negative integer (2023 14.9.39.3 rule 34).
data division.
working-storage section.
01 s pic x dynamic length.
procedure division.
    set size of s to -1
    goback.
