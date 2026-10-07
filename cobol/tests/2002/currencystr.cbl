identification division.
program-id. currencystr.
*> CURRENCY SIGN IS literal WITH PICTURE SYMBOL literal (2023 12.3.7
*> rules 23, 26-27; standard-queue item 14): the symbol stands in the
*> PICTURE for a currency string of several characters, the first
*> occurrence adding the string's length to the item (13.18.40.4, cs),
*> fixed and floating insertion placing the string, de-editing and
*> NUMVAL-C reading it, a 31-digit item edited the same way.  No oracle:
*> GnuCOBOL 4 does not implement the separate currency string.
environment division.
configuration section.
special-names.
    currency sign is "EUR" with picture symbol "$".
data division.
working-storage section.
01  v        pic 9(5)v99 value 1234.5.
01  e1       pic $$$$9.99.
01  e2       pic $9,999.99.
01  e3       pic $$,$$9.99-.
01  e4       pic $(6).99.
01  n        pic 9(5)v99.
01  w        pic s9(20)v99 value -1234.5.
01  e5       pic $(22).99-.
01  s        pic x(20) value "EUR 1,234.50".
procedure division.
    move v to e1 display "[" e1 "] " function length(e1)
    move v to e2 display "[" e2 "]"
    move 5 to e3 display "[" e3 "]"
    move 0 to e4 display "[" e4 "]"
    move v to e4 display "[" e4 "]"
    move e1 to n display "de-edit      " n
    move w to e5 display "[" e5 "]"
    move e5 to w display "wide de-edit " w
    compute n = function numval-c(s) display "numval-c     " n
    stop run.
end program currencystr.
