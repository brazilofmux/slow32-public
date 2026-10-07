*> The 2014 international date and time functions (2023 15.3.1-15.3.3
*> formats; 15.17 COMBINED-DATETIME, 15.38 FORMATTED-CURRENT-DATE, 15.39
*> FORMATTED-DATE, 15.40 FORMATTED-DATETIME, 15.41 FORMATTED-TIME, 15.48
*> INTEGER-OF-FORMATTED-DATE, 15.79 SECONDS-FROM-FORMATTED-TIME, 15.80
*> SECONDS-PAST-MIDNIGHT, 15.92 TEST-FORMATTED-DATETIME): the six date
*> formats (calendar, ordinal, week; basic and extended), the time
*> formats with integer and fractional seconds, local, UTC (Z, the time
*> adjusted by the offset, the date rolling with it) and offset (+hh:mm),
*> combined formats; the scanning functions and TEST's position of the
*> first error (the text's own examples); the clock pinned by
*> dtformat.env (the offset is then zero).  National formats, the
*> fraction of the current time and SECONDS-PAST-MIDNIGHT are in
*> dtformat2.  GnuCOBOL agrees but for what docs/oracles.md records.
*> docs/conformance/functions.md
identification division.
program-id. dtformat.
data division.
working-storage section.
01 n pic 9(10).
01 s pic 9(5)v99 value 45296.5.
01 r pic x(30).
01 c pic 9(10)v9(9).
01 p pic 99.
01 q pic 9(5)v9(4).
01 d8 pic x(8).
procedure division.
    move function integer-of-date(20261007) to n
    display "n " n
    display "[" function formatted-date("YYYYMMDD", n) "][" function formatted-date("YYYY-MM-DD", n) "]"
    display "[" function formatted-date("YYYYDDD", n) "][" function formatted-date("YYYY-DDD", n) "]"
    display "[" function formatted-date("YYYYWwwD", n) "][" function formatted-date("YYYY-Www-D", n) "]"
    display "[" function formatted-date("YYYY-Www-D", function integer-of-date(20270101)) "]"
    display "[" function formatted-date("YYYY-Www-D", function integer-of-date(20241230)) "]"
    display "[" function formatted-time("hhmmss", s) "][" function formatted-time("hh:mm:ss", s) "]"
    display "[" function formatted-time("hhmmss.ss", s) "][" function formatted-time("hh:mm:ss.sss", s) "]"
    display "[" function formatted-time("hhmmssZ", s, -300) "][" function formatted-time("hh:mm:ssZ", s, 600) "]"
    display "[" function formatted-time("hhmmss+hhmm", s, -300) "][" function formatted-time("hh:mm:ss+hh:mm", s, 90) "]"
    display "[" function formatted-time("hh:mm:ss+hh:mm", s) "][" function formatted-time("hh:mm:ssZ", s) "]"
    display "[" function formatted-datetime("YYYYMMDDThhmmss", n, s) "]"
    display "[" function formatted-datetime("YYYY-MM-DDThh:mm:ss.ss+hh:mm", n, s, -420) "]"
    display "[" function formatted-datetime("YYYY-MM-DDThh:mm:ssZ", n, 600, 660) "]"
    display "[" function formatted-datetime("YYYY-DDDThh:mm:ssZ", n, 86000, -600) "]"
    move function integer-of-formatted-date("YYYYMMDD", "20261007") to n display "integer-of-formatted-date " n
    move function integer-of-formatted-date("YYYY-Www-D", "2026-W41-3") to n display "  of a week date " n
    move function integer-of-formatted-date("YYYYDDDThhmmss", "2026280T123456") to n display "  of a combined one " n
    move function seconds-from-formatted-time("hhmmss", "123456") to q display "seconds-from-formatted-time " q
    move function seconds-from-formatted-time("hh:mm:ss.ss", "12:34:56.78") to q display "  with a fraction " q
    move function seconds-from-formatted-time("YYYYMMDDThhmmssZ", "20261007T123456Z") to q display "  of a combined one " q
    move function test-formatted-datetime("YYYYMMDD", "20051314") to p display "test 20051314: " p
    move function test-formatted-datetime("YYYYMMDD", "15990316") to p display "test 15990316: " p
    move function test-formatted-datetime("YYYYMMDD", "20260229") to p display "test 20260229: " p
    move function test-formatted-datetime("YYYYMMDD", "20240229") to p display "test 20240229: " p
    move function test-formatted-datetime("YYYY-MM-DD", "2026/10/07") to p display "test 2026/10/07: " p
    move function test-formatted-datetime("hh:mm:ss+hh:mm", "12:34:56-05:00") to p display "test -05:00: " p
    move function test-formatted-datetime("hh:mm:ss+hh:mm", "12:34:56-05:60") to p display "test -05:60: " p
    move function test-formatted-datetime("hh:mm:ss+hh:mm", "12:34:56000:00") to p display "test 000:00: " p
    move function test-formatted-datetime("hh:mm:ss+hh:mm", "12:34:56000:30") to p display "test 000:30: " p
    move function test-formatted-datetime("hhmmss", "243456") to p display "test 243456: " p
    move function test-formatted-datetime("hhmmss.sss", "123456x78") to p display "test 123456x78: " p
    move function test-formatted-datetime("YYYY-Www-D", "2026-W53-1") to p display "test 2026-W53-1: " p
    move function test-formatted-datetime("YYYY-Www-D", "2025-W53-1") to p display "test 2025-W53-1: " p
    move function test-formatted-datetime("YYYY-Www-D", "2020-W53-7") to p display "test 2020-W53-7: " p
    move function test-formatted-datetime("YYYYDDD", "2026366") to p display "test 2026366: " p
    move function test-formatted-datetime("YYYYDDD", "2024366") to p display "test 2024366: " p
    move function test-formatted-datetime("YYYY-MM-DDThh:mm:ssZ", "2026-10-07T12:34:56Z") to p display "test combined: " p
    move function test-formatted-datetime("YYYY-MM-DDThh:mm:ssZ", "2026-10-07 12:34:56Z") to p display "test combined, a space for T: " p
    move function combined-datetime(n, s) to c display "combined-datetime " c
    display "[" function formatted-current-date("YYYY-MM-DDThh:mm:ss+hh:mm") "]"
    stop run.
