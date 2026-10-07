identification division.
program-id. sameas.
*> SAME AS (2023 13.18.49; standard-queue item 11): an elementary item's
*> and a level 01 group's description coded in place of the clause, the
*> subordinates following with their levels adjusted (general rules
*> 1-2), under a group, at level 77, inside an OCCURS, and past level 49
*> (rule 2c) -- as a TYPE expanding past 49 is (13.18.57.4 rule 2c).
*> GnuCOBOL 4 agrees.
data division.
working-storage section.
01  addr.
    05  street   pic x(12) value "main st".
    05  city     pic x(8) value "springf".
    05  zip      pic 9(5) value 12345.
    88  local-zip value 12345.
01  amount      pic s9(5)v99 value -12.5.
01  home        same as addr.
01  work.
    05  wname    pic x(6) value "acme".
    05  waddr    same as addr.
    05  wamount  same as amount.
77  total       same as amount.
01  tbl.
    05  row occurs 2.
        10  r-addr same as addr.
01  pair typedef.
    05  lo.
        10  x1 pic 9(2) value 1.
        10  x2 pic 9(2) value 2.
    05  hi pic 9(2) value 3.
01  pr.
    05  lo.
        10  y1 pic 9(2) value 4.
        10  y2 pic 9(2) value 5.
    05  hi pic 9(2) value 6.
01  deep.
    05  d1.
        48  d2 type pair.
        48  d3 same as pr.
        48  d4 pic x value "z".
procedure division.
    display "home:  " home
    move "elm rd" to street of waddr
    display "work:  " work
    if local-zip of waddr display "zip ok" end-if
    move 99 to total display "total: " total
    move addr to r-addr(2)
    display "row 2: " r-addr(2)
    display "deep:  " deep
    display "past 49: " x1 of d2 " " hi of d3
    move 9 to y2 of d3
    display "deep:  " deep
    stop run.
end program sameas.
