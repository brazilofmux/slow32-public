identification division.
program-id. pr.
*> PROCEDURE DIVISION RAISING (exception propagation, COBOL 2002):
*> refused by name; it was a parse error.
data division.
working-storage section.
01 a pic 9.
procedure division raising ec-size.
    display a
    goback.
