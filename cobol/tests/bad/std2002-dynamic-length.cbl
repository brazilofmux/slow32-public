identification division.
program-id. p-std2002-dynamic-length.
*> The DYNAMIC LENGTH clause is COBOL 2014; under -std=2002 it is refused naming the switch.
data division.
working-storage section.
01 s pic x dynamic length.
procedure division.
    display s
    goback.
