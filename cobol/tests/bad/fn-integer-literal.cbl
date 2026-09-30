identification division.
program-id. p.
data division.
working-storage section.
01 n5 pic 9(5) value 12345.
01 d1 pic 9v9 value 2.5.
01 x pic x(10) value "4".
01 r pic s9(9)v9(4).
procedure division.
    compute r = function annuity(0.1, 2.5)
    stop run.
