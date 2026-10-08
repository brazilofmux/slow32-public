*> A GLOBAL screen (13.18.27; 13.17.3 rule 2) displayed from a contained
*> program, placed by AT; SET screen-name ATTRIBUTE ... ON/OFF (14.9.39
*> format 6) changing every slot before the next DISPLAY (item 38).
*> No oracle: screens need a tty.
identification division.
program-id. screenglobal.
data division.
working-storage section.
01 msg pic x(10) value "outer-item".
screen section.
01 gscr is global.
   05 line 2 column 3 value "Global:".
   05 column plus 1 pic x(10) from msg underline.
procedure division.
    display gscr.
    set gscr attribute underline off highlight on.
    call "inner".
    stop run.
identification division.
program-id. inner.
procedure division.
    display gscr at line 5 column 1.
end program inner.
end program screenglobal.
