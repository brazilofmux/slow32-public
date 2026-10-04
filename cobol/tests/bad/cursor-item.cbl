identification division.
program-id. cursorbad.
*> The CURSOR item is six digits, line and column three each (2023
*> 12.3.7 rule 29); the four-character form some compilers also take is
*> not the standard's.
environment division.
configuration section.
special-names.
    cursor is cur-pos.
data division.
working-storage section.
01 cur-pos pic 9(4).
01 v pic x(3).
screen section.
01 s1.
    05 line 1 column 1 pic x(3) using v.
procedure division.
    accept s1
    goback.
