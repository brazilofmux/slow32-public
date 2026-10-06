identification division.
program-id. alo.
*> ALPHABET ... IS LOCALE (COBOL 2002 locale support): refused by name.
environment division.
configuration section.
special-names.
    alphabet a1 is locale.
procedure division.
    display 'x'
    goback.
