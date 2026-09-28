identification division.
program-id. nattoa.
*> A national item is not moved to an alphanumeric one (2023 14.9.25);
*> FUNCTION DISPLAY-OF converts.
data division.
working-storage section.
01  n pic n(4) value n"abc".
01  a pic x(4).
procedure division.
    move n to a
    stop run.
