identification division.
program-id. dupcl.
*> Each clause of a screen entry is written once (2023 13.17.2); a second
*> LINE used to replace the first without a word.
data division.
working-storage section.
01 a pic x(3).
screen section.
01 s1.
   05 line 1 line 2 col 1 pic x(3) using a.
procedure division.
    accept s1
    goback.
