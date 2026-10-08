*> No gcobol: the test depends on COB_CURRENT_DATE, the command line as GnuCOBOL counts it, or a print device -- the harness skips it there.
identification division.
program-id. fixclock.
*> COB_CURRENT_DATE fixes the clock (cobol ISSUES-45): the .env beside
*> this source sets 2024/02/29 23:59:59, a leap day and a Thursday, and
*> every date field must read it.  GnuCOBOL reads the same variable and
*> prints the same lines.  Only the fields both runtimes fix are shown:
*> GnuCOBOL 4.0 lets the real hundredths through, where this runtime
*> reads zero (section C), so TIME stops at the second here.
data division.
working-storage section.
01 d6   pic 9(6).
01 d5   pic 9(5).
01 t8   pic 9(8).
01 w1   pic 9.
01 cdt  pic x(21).
01 cd8  pic x(8).
01 ct6  pic x(6).
procedure division.
    accept d6 from date
    accept d5 from day
    accept t8 from time
    accept w1 from day-of-week
    move function current-date to cdt
    move cdt(1:8) to cd8
    move cdt(9:6) to ct6
    display "date " d6 " day " d5 " dow " w1
    display "time " t8(1:6)
    display "current-date " cd8 " " ct6
    stop run.
