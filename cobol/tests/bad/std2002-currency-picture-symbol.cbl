identification division.
program-id. cps.
*> A currency string with PICTURE SYMBOL (COBOL 2002): refused by name.
environment division.
configuration section.
special-names.
    currency sign is "EUR" with picture symbol "$".
data division.
working-storage section.
01 a pic $$9.
procedure division.
    move 1 to a
    goback.
