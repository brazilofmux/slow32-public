identification division.
program-id. datefn.
*> The calendar functions of the 1989 addendum to COBOL 85:
*> INTEGER-OF-DATE, DATE-OF-INTEGER, DAY-OF-INTEGER, INTEGER-OF-DAY.
*> Integer 1 is 1601-01-01; an invalid date or day gives 0.
data division.
working-storage section.
01  ymd              pic 9(8).
01  n                pic 9(7).
01  nb               pic s9(8) comp.
01  jd               pic 9(7).
01  i                pic 9(3) comp.
procedure division.
main.
    display 'first:    ' function integer-of-date(16010101).
    display 'y2k:      ' function integer-of-date(20000101).
    display 'leap day: ' function integer-of-date(20000229).
    display 'not leap: ' function integer-of-date(19000229).
    display 'bad month:' function integer-of-date(20001301).
    display 'last:     ' function integer-of-date(99991231).
    move 20240315 to ymd.
    move function integer-of-date(ymd) to n.
    display 'n:        ' n.
    move function integer-of-date(ymd) to nb.
    display 'nb:       ' nb.
    add 1 to nb.
    move function date-of-integer(nb) to ymd.
    display 'next day: ' ymd.
    move function day-of-integer(nb) to jd.
    display 'julian:   ' jd.
    move function integer-of-day(jd) to n.
    display 'and back: ' n.
    display 'date 1:   ' function date-of-integer(1).
    display 'date 0:   ' function date-of-integer(0).
    display 'day 1:    ' function day-of-integer(1).
    display 'day 0:    ' function day-of-integer(0).
    display 'day 366:  ' function integer-of-day(2023366).
    display 'day 366:  ' function integer-of-day(2024366).
*>  every year end round-trips
    move 0 to nb.
    perform varying i from 1 by 1 until i > 199
        compute ymd = (1600 + i * 42) * 10000 + 1231
        move function integer-of-date(ymd) to n
        if function date-of-integer(n) not = ymd add 1 to nb end-if
    end-perform.
    display 'mismatches: ' nb.
    stop run.
