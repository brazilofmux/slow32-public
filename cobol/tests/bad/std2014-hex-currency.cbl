identification division.
program-id. p-hex-currency.
*> A hexadecimal literal is not the currency symbol (2014 E.2 item 10; 2023 12.3.7 rule 24).
environment division.
configuration section.
special-names.
    currency sign is x"24".
procedure division.
    display "x"
    stop run.
