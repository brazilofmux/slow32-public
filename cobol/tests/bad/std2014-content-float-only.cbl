identification division.
program-id. p-content-float-only.
*> SET CONTENT OF a DISPLAY item TO FLOAT-INFINITY: a floating-point item
*> (2023 14.9.39.3 rule 32).
data division.
working-storage section.
01 p pic 9(3).
procedure division.
    set content of p to float-infinity
    goback.
