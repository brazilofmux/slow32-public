identification division.
program-id. p-float-byvalue.
*> A standard floating-point item BY VALUE: a float is not passed by value
*> here (Micro Focus: CALL rules).
data division.
working-storage section.
01 f usage float-decimal-34.
procedure division.
    call "x" using by value f
    goback.
