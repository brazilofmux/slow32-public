identification division.
program-id. p-std2014-dynl-pic.
*> DYNAMIC LENGTH takes PICTURE X or N, one symbol (2023 13.18.19.3 rule 1).
data division.
working-storage section.
01 s pic x(3) dynamic length.
procedure division.
    display s
    goback.
