identification division.
program-id. p-std2014-dynl-set-above.
*> SET SIZE OF TO above the maximum size (2023 14.9.39.3 rule 34).
data division.
working-storage section.
01 s pic x dynamic length limit 8.
procedure division.
    set size of s to 9
    goback.
