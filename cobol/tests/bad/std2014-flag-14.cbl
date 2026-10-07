>>FLAG-14 ALL ON
identification division.
program-id. p-std2014-flag-14.
*> FLAG-14 is COBOL 2023 (7.3.15).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
