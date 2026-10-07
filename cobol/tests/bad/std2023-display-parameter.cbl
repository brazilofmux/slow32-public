>>DISPLAY PARAMETER 3
identification division.
program-id. p-std2023-display-parameter.
*> DISPLAY PARAMETER names a compilation variable (7.3.12.2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
