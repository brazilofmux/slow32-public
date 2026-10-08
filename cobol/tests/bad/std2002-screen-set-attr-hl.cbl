identification division.
program-id. p-std2002-screen-set-attr-hl.
*> SET ATTRIBUTE: not HIGHLIGHT and LOWLIGHT together (14.9.39.3 rule 16).
data division.
working-storage section.
01 n pic 99 value 3.
01 sn pic s99 value 3.
01 x pic x(4) value "abcd".
01 tbl.
   05 el pic x(2) occurs 2 times.
screen section.
01 s.
   05 line 1 column 1 value "a".
procedure division.
    set s attribute highlight on lowlight off.
    stop run.
