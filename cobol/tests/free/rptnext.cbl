*> NEXT GROUP in its three forms, and LINE NEXT PAGE. The detail
*> carries NEXT GROUP PLUS 1 (a spaced listing); the ACCT footing
*> NEXT GROUP NEXT PAGE ends the page after each account; a detail
*> whose LINE says NEXT PAGE begins one. The program reads its own
*> print file back. Oracle: GnuCOBOL, divergences documented.
identification division.
program-id. rptnext.
environment division.
input-output section.
file-control.
    select prt assign to 'rptnext.prn' organization line sequential.
    select chk assign to 'rptnext.prn' organization line sequential.
data division.
file section.
fd  prt report is r.
fd  chk.
01  chk-line pic x(30).
working-storage section.
77  i     pic 9.
77  acct  pic 99.
77  amt   pic 9(3)v99.
77  eof-f pic 9 value 0.
report section.
rd  r
    controls are acct
    page limit 12 heading 1 first detail 3 last detail 9 footing 10.
01  type page heading line 1.
    05  column 1 value 'PAGE'.
    05  column 6 pic z9 source page-counter.
01  det type detail line plus 1 next group plus 1.
    05  column 1 pic 99 source acct.
    05  column 5 pic zz9.99 source amt.
01  cfa type control footing acct line plus 1 next group next page.
    05  column 1 value 'TOTAL'.
    05  column 7 pic zzz9.99 sum amt.
01  big type detail line next page.
    05  column 1 value 'FRESH PAGE DETAIL'.
procedure division.
    open output prt.
    initiate r.
    move 10 to acct.
    move 1.50 to amt.
    generate det.
    move 2.25 to amt.
    generate det.
    move 20 to acct.
    move 4.00 to amt.
    generate det.
    generate big.
    terminate r.
    close prt.
    open input chk.
    perform until eof-f = 1
        read chk
            at end move 1 to eof-f
            not at end display '[' chk-line ']'
        end-read
    end-perform.
    close chk.
    stop run.
end program rptnext.
