*> A reference modification whose start is an expression with a
*> subscripted operand, r(p + w - len(i):...).  Addressing len(i) used
*> r11, the register the outer item's offset accumulates in, and left its
*> own offset there: the start came out late by (i - 1) times len's size.
*> Found by the csv2fw port in majesty (2026-09-30); every line here was
*> wrong before.  The outer item is subscripted too, in the last lines.
identification division.
program-id. refmodsub.
data division.
working-storage section.
01  r                           pic x(12).
01  fi                          pic 99 value 2.
01  fw                          pic 99 value 2.
01  fpos                        pic 9(4) comp value 6.
01  fields-area.
    05 fld occurs 4.
       10 fld-len               pic 9(5) comp.
       10 fld-text              pic x(8).
procedure division.
    move 1 to fld-len(2)  move 3 to fld-len(3)
    move "abcdefgh" to fld-text(2)  move "ABCDEFGH" to fld-text(3)
    move all "." to r  move "A" to r(fpos + fw - fld-len(fi):1)
    display "1 [" r "]"
    move all "." to r  move "B" to r(fpos - fld-len(fi):1)
    display "2 [" r "]"
    move all "." to r  move "C" to r(fw - fld-len(fi) + fpos:fld-len(fi))
    display "3 [" r "]"
    move 3 to fi
    move all "." to r  move fld-text(fi)(fld-len(fi):2) to r(fld-len(fi) + fld-len(2):2)
    display "4 [" r "]"
    display "5 [" fld-text(fi - 1)(fld-len(fi) - fld-len(2):3) "]"
    stop run.
