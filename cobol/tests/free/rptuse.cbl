*> USE BEFORE REPORTING with SUPPRESS PRINTING: the declarative runs
*> before its group presents, may change what prints, and may
*> suppress the group entirely -- a suppressed footing moves no paper
*> while its counter still rolls and resets. GENERATE report-name
*> (summary reporting) drives it: details never print, sums do.
*> UPON restricts one counter to one detail; RESET ON FINAL keeps a
*> running total across breaks. Oracle: GnuCOBOL.
identification division.
program-id. rptuse.
environment division.
input-output section.
file-control.
    select prt assign to 'rptuse.prn' organization line sequential.
    select chk assign to 'rptuse.prn' organization line sequential.
data division.
file section.
fd  prt report is r.
fd  chk.
01  chk-line pic x(34).
working-storage section.
77  i     pic 9.
77  dept  pic 9.
77  amt   pic 9(3)v99.
77  eof-f pic 9 value 0.
report section.
rd  r
    controls are final dept
    page limit 30 heading 1 first detail 3 last detail 26 footing 28.
01  type page heading line 1 column 1 value 'SUMMARY'.
01  d1 type detail line plus 1.
    05  column 1 pic 9 source dept.
    05  column 5 pic zz9.99 source amt.
01  d2 type detail line plus 1 column 1 value 'OTHER'.
01  cfd type control footing dept line plus 1.
    05  column 1 value 'DEPT'.
    05  column 6 pic 9 source dept.
    05  column 9 pic zzz9.99 sum amt.
    05  only1 column 18 pic zzz9.99 sum amt upon d1.
    05  runn column 27 pic zzz9.99 sum amt reset on final.
01  type control footing final line plus 2.
    05  column 1 value 'GRAND'.
    05  column 9 pic zzz9.99 sum runn.
procedure division.
declaratives.
sup-sec section. use before reporting cfd.
sup-para.
    if dept = 2 suppress printing.
end declaratives.
main section.
m1.
    open output prt.
    initiate r.
    move 1 to dept.
    move 1.00 to amt.
    generate d1.
    move 2.00 to amt.
    generate d2.
    move 3.00 to amt.
    generate r.
    move 2 to dept.
    move 4.00 to amt.
    generate d1.
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
end program rptuse.
