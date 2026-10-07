identification division.
program-id. p-std2014-packed-no-sign.
*> PACKED-DECIMAL WITH NO SIGN is COBOL 2023 (13.18.60).
data division.
working-storage section.
01 p pic 9(5) packed-decimal with no sign.
procedure division.
    display p
    stop run.
