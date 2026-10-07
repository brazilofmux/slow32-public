*> The 2014 date and time functions, the part GnuCOBOL 4 does not take
*> or cannot pin: national formats and data (15.38.3 rule 1 and its
*> siblings: the result national when the format is), FORMATTED-CURRENT-
*> DATE's fraction (to the hundredth, 15.38.4 rule 2) and SECONDS-PAST-
*> MIDNIGHT (15.80, to the hundredth), both from the clock dtformat2.env
*> pins (GnuCOBOL's SECONDS-PAST-MIDNIGHT reads the real clock through
*> COB_CURRENT_DATE); DECIMAL-POINT IS COMMA, the comma the fraction's
*> separator in the format and the extended data (15.3.3.2); EC-ARGUMENT-
*> FUNCTION for data out of its format.
*> No oracle: GnuCOBOL 4 refuses a national format, and the clock.
*> docs/conformance/functions.md
identification division.
program-id. dtformat2.
environment division.
configuration section.
special-names.
    decimal-point is comma.
data division.
working-storage section.
01 n pic 9(10).
01 s pic 9(5)v99 value 45296,5.
01 p pic 99.
01 q pic 9(5)v9(4).
01 nn pic n(10).
01 spm pic 9(5)v99.
procedure division.
declaratives.
d section.
    use after exception condition ec-argument-function.
d1.
    display "  EC-ARGUMENT-FUNCTION".
end declaratives.
main section.
m1.
    move function integer-of-date(20261007) to n
    display "[" function formatted-date(n"YYYY-MM-DD", n) "][" function formatted-time(n"hh:mm:ss,ss", s) "]"
    display "[" function formatted-time("hhmmss,sss", s) "][" function formatted-time("hh:mm:ss,s", s) "]"
    move n"20261007" to nn
    move function test-formatted-datetime(n"YYYYMMDD", nn) to p display "national test: " p
    move function integer-of-formatted-date(n"YYYYMMDD", nn) to n display "national date: " n
    move n"2026-W41-3" to nn
    move function integer-of-formatted-date(n"YYYY-Www-D", nn) to n display "national week date: " n
    move function seconds-from-formatted-time("hh:mm:ss,ss", "12:34:56,78") to q display "seconds with a comma: " q
    move function test-formatted-datetime("hh:mm:ss,ss", "12:34:56.78") to p display "test of a point: " p
    display "[" function formatted-current-date("YYYYDDDThhmmss,ssZ") "]"
    display "[" function formatted-current-date("YYYY-MM-DDThh:mm:ss,sss+hh:mm") "]"
    move function seconds-past-midnight to spm display "seconds-past-midnight " spm
    >>turn ec-argument-function checking on
    move function integer-of-formatted-date("YYYYMMDD", "20261307") to n display "bad date: " n
    move function seconds-from-formatted-time("hhmmss", "2534") to q display "bad time: " q
    move function formatted-date("YYYYMMDD", 0) to nn display "bad integer date: [" nn "]"
    stop run.
