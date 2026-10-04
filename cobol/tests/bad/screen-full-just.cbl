identification division.
program-id. fjbad.
*> FULL and JUSTIFIED do not go together on a screen item (2023
*> 13.18.60.3 rule 8).
data division.
working-storage section.
01 v pic x(3).
screen section.
01 s1.
    05 line 1 column 1 pic x(3) using v full justified.
procedure division.
    accept s1
    goback.
