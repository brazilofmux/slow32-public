identification division.
program-id. exttrim.
*> -warn-extensions: FUNCTION TRIM is COBOL 2014 (BP-E27).
data division.
working-storage section.
01 s pic x(4) value " ab ".
procedure division.
    display function trim(s)
    stop run.
