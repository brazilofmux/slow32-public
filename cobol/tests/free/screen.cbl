*> SCREEN SECTION on the term service: usescreen.cbl's shape.  BLANK
*> SCREEN, VALUE, PIC TO, PIC FROM, an edited FROM with BLANK WHEN
*> ZERO, HIGHLIGHT, USING with AUTO; DISPLAY, ACCEPT, DISPLAY.  The
*> keys come from screen.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. screen.
data division.
working-storage section.
77  amount-in         pic x(6).
77  amount            pic s9(3)v99 comp-5 value 0.
77  code-in           pic xx value 'ab'.
77  name-in           pic x(8).
01  screen-size.
    05  no-of-lines   usage binary-char unsigned.
    05  no-of-columns usage binary-char unsigned.
screen section.
01  screen-in.
    05  blank screen.
    05  line 2 column 30 value 'Using screen section!' highlight.
    05  line 3 column 30 pic x(6) to amount-in.
    05  line 4 column 30 pic xx using code-in auto.
    05  line 5 column 30 pic x(8) to name-in.
    05  line 7 column 30 pic zz9 from no-of-lines.
    05  line 8 column 30 pic zz9 from no-of-columns.
01  screen-out.
    05  line 2 column 30 value 'Using screen section!'.
    05  line 3 column 30 pic $zz9.99- blank when zero from amount.
    05  line 4 column 30 pic xx from code-in.
    05  line 5 column 30 pic x(8) from name-in.
procedure division.
    call 'CBL_GET_SCR_SIZE' using no-of-lines no-of-columns.
    display screen-out.
    accept screen-in.
    move amount-in to amount.
    display screen-out.
    display 'amount-in=[' amount-in '] amount=' amount ' code=[' code-in
            '] name=[' name-in '] size=' no-of-lines ' x ' no-of-columns.
    move -1.5 to amount.
    display screen-out.
    stop run.
end program screen.
