identification division.
program-id. p-std2023-packed-no-sign-s.
*> PACKED-DECIMAL WITH NO SIGN takes a PICTURE without S (2023 13.18.60 GR 25).
data division.
working-storage section.
01 p pic s9(5) packed-decimal with no sign.
procedure division.
    display p
    stop run.
