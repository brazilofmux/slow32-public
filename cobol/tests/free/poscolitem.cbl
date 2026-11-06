*> COL is no reserved word before 2002: a program may have an item of that
*> name, and DISPLAY of it is the item, not a phrase.  AT COL is the item
*> as rrcc.  The ANSI stream is the expected output.  No oracle.
identification division.
program-id. poscolitem.
data division.
working-storage section.
01 col pic 9(4) value 0305.
01 other pic x value "x".
procedure division.
    display col
    display other col
    display "y" at col
    stop run.
end program poscolitem.
