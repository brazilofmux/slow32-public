*> ACCEPT ... FROM DATE YYYYMMDD and DAY YYYYDDD (COBOL 2002; 2023
*> 14.9.1.4): the four-digit year, beside the two-digit forms.  The
*> clock is pinned by acceptyyyy.env.  docs/conformance/accept.md
identification division.
program-id. acceptyyyy.
data division.
working-storage section.
01 d6 pic 9(6).
01 d8 pic 9(8).
01 y5 pic 9(5).
01 y7 pic 9(7).
procedure division.
    accept d6 from date
    accept d8 from date yyyymmdd
    accept y5 from day
    accept y7 from day yyyyddd
    display d6 " " d8 " " y5 " " y7
    stop run.
