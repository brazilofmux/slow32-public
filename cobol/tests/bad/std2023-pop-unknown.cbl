>>POP FROBNICATE
identification division.
program-id. p-std2023-pop-unknown.
*> POP names a compiler directive (7.3.20.2).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
