identification division.
program-id. fromto.
*> FROM with TO in one screen entry shows one item and keys into
*> another (2023 13.17.2).  It compiled as TO alone, the FROM dropped
*> without a word; now refused as not implemented.
data division.
working-storage section.
01 a pic x(3) value 'abc'.
01 b pic x(3).
screen section.
01 s1.
   05 line 1 col 1 pic x(3) from a to b.
procedure division.
    accept s1
    goback.
