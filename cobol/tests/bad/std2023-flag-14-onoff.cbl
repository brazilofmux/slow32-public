>>FLAG-14 ALL
identification division.
program-id. p-std2023-flag-14-onoff.
*> FLAG-14 ends with ON or OFF (7.3.15.2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
