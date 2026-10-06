identification division.
program-id. pfirst.
*> LINE PLUS and COLUMN PLUS count from the screen item before; the first
*> elementary item of a screen has none (2023 13.18.35.3 rule 13,
*> 13.18.14.3 rule 13).  Accepted before.
data division.
working-storage section.
01 a pic x(3).
screen section.
01 s1.
   05 line plus 1 col 1 pic x(3) using a.
procedure division.
    accept s1
    goback.
