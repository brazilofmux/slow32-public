identification division.
program-id. hilo.
*> HIGHLIGHT and LOWLIGHT are alternatives of one element of the format
*> (2023 13.17.2); both in one entry were accepted, HIGHLIGHT winning.
data division.
working-storage section.
01 a pic x(3).
screen section.
01 s1.
   05 line 1 col 1 pic x(3) using a highlight lowlight.
procedure division.
    accept s1
    goback.
