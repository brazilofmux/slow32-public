identification division.
program-id. accout.
*> ACCEPT of a screen whose items are all output (FROM, VALUE) is not
*> allowed (2023 14.9.1.3 rule 4); it compiled, and at run time ended
*> with status 8000.
data division.
working-storage section.
01 a pic x(3) value 'abc'.
screen section.
01 s1.
   05 line 1 col 1 pic x(3) from a.
   05 line 2 col 1 value 'output only'.
procedure division.
    accept s1
    goback.
