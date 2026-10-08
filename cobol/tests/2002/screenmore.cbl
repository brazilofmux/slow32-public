*> The screen section leftovers (docs/plans/standard-queue.md item 38): LINE,
*> COLUMN and a colour from identifiers, COLUMN MINUS, FROM x TO y, FROM a
*> numeric literal through the PICTURE, OCCURS over a table's elements,
*> BLANK LINE, BLANK SCREEN's default colours, the screen placed by AT LINE
*> COLUMN, DISPLAY ... ON EXCEPTION for a field off the screen and an
*> overlap (EC-SCREEN). No oracle: screens need a tty.
identification division.
program-id. screenmore.
data division.
working-storage section.
01 ln pic 99 value 3.
01 cl pic 99 value 10.
01 clr pic 9 value 2.
01 shown pic x(5) value "shown".
01 keyed pic x(5) value "     ".
01 tbl.
   05 elem pic x(4) occurs 3 times.
01 amt pic 9(3)v99 value 12.5.
01 off-line pic 99 value 90.
screen section.
01 scr.
   05 blank screen background-color 1.
   05 line ln column cl value "Dyn".
   05 line plus 1 column plus 2 pic x(5) from shown to keyed.
   05 line plus 1 column minus 3 value "Back".
   05 blank line line 7 column 20 value "Blanked".
   05 line 8 column 1 pic zz9.99 from 7.5.
   05 line 9 column 1 pic 9(3) from 42.
   05 line 10 column plus 2 pic x(4) using elem occurs 3 times.
   05 line ln column 40 foreground-color clr value "Col".
01 scr-off.
   05 line off-line column 1 value "far".
   05 line 1 column 1 value "near".
   05 line 1 column 3 value "lap".
procedure division.
    move "aaaa" to elem (1). move "bbbb" to elem (2). move "cccc" to elem (3).
    display scr.
    display scr at line 2 column 5.
    display scr-off on exception display "exception" not on exception display "fine" end-display.
    accept scr.
    display "keyed=[" keyed "] elem(2)=[" elem (2) "]".
    stop run.
