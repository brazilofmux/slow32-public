*> A positioned DISPLAY of a reference-modified part whose length is
*> computed (BP-E7's DISPLAY ... AT): the part's characters, as many as
*> the length says when the statement runs.  ACAS's pl015 and sl020
*> (cobol ISSUES-124) show a window of a report line this way:
*> "display line-7-19 (Screen-Start:Screen-End) at 0801".
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. posrmlen.
data division.
working-storage section.
01  body   pic x(20) value 'ABCDEFGHIJKLMNOPQRST'.
01  st     pic 99 value 3.
01  ln     pic 99 value 4.
procedure division.
    display body (st:ln) at 0101
    move 10 to st move 6 to ln
    display body (st:ln) at 0201 "|" at 0210
    display body (st + 1:ln - 3) at 0301
    stop run.
