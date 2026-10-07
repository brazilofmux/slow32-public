>>FLAG-14 FROB ON
identification division.
program-id. p-std2023-flag-14-option.
*> FLAG-14 takes its options (7.3.15.2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
