identification division.
program-id. grpjust.
*> JUSTIFIED belongs to an elementary item (2023 13.18.32.3 rule 1), and
*> a group screen entry has no JUSTIFIED or BLANK WHEN ZERO in its
*> format (13.17.2 format 1).  Accepted and ignored before.
data division.
working-storage section.
01 a pic x(3).
screen section.
01 s1.
   05 g justified.
      10 line 1 col 1 pic x(3) using a.
procedure division.
    accept s1
    goback.
