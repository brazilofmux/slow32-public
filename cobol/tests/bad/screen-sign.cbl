identification division.
program-id. signbad.
*> SIGN on a screen item wants a numeric PICTURE with an S (2023
*> 13.18.52.3 rule 1).
data division.
working-storage section.
01 v pic 9(3).
screen section.
01 s1.
    05 line 1 column 1 pic 9(3) using v sign leading separate.
procedure division.
    accept s1
    goback.
