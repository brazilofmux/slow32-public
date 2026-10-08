identification division.
program-id. p-std2014-dynl-usage.
*> DYNAMIC LENGTH with a usage other than DISPLAY or NATIONAL (2023 13.18.19.3 rule 1).
data division.
working-storage section.
01 s pic x dynamic length usage comp-5.
procedure division.
    display s
    goback.
