*> Report Writer's expensive half: CONTROL FINAL DEPT ACCT, a heading
*> and footing at each level, the footings printing the PRIOR control
*> values, SUM rolling ACCT -> DEPT -> GRAND, GROUP INDICATE on the
*> detail's dept column, REPORT HEADING and FOOTING. The program
*> reads its own print file back so the oracle diffs every byte.
identification division.
program-id. rptctl.
environment division.
input-output section.
file-control.
    select prt assign to 'rptctl.prn' organization line sequential.
    select chk assign to 'rptctl.prn' organization line sequential.
data division.
file section.
fd  prt report is r.
fd  chk.
01  chk-line pic x(40).
working-storage section.
77  i     pic 9.
77  dept  pic 9.
77  acct  pic 99.
77  amt   pic 9(3)v99.
77  eof-f pic 9 value 0.
01  rows-lit pic x(48) value '110001001100020012000300210004002300005023000125'.
01  rows redefines rows-lit.
    05  row occurs 6.
        10  r-dept pic 9.
        10  r-acct pic 99.
        10  r-amt  pic 9(3)v99.
report section.
rd  r
    controls are final dept acct
    page limit 40 heading 1 first detail 3 last detail 36 footing 38.
01  type report heading line 1 column 1 value 'THE LEDGER'.
01  type page heading line 2.
    05  column 1 value 'DEPT ACCT      AMT'.
01  chd type control heading dept line plus 1.
    05  column 1 value '** DEPT'.
    05  column 9 pic 9 source dept.
01  det type detail line plus 1.
    05  column 2 pic 9 source dept group indicate.
    05  column 7 pic 99 source acct.
    05  column 12 pic zz9.99 source amt.
01  cfa type control footing acct line plus 1.
    05  column 3 value 'ACCT'.
    05  column 8 pic 99 source acct.
    05  a-tot column 11 pic zzz9.99 sum amt.
01  cfd type control footing dept line plus 1.
    05  column 1 value 'DEPT'.
    05  column 6 pic 9 source dept.
    05  d-tot column 10 pic zzzz9.99 sum a-tot.
01  type control footing final line plus 2.
    05  column 1 value 'GRAND'.
    05  column 9 pic zzzzz9.99 sum d-tot.
01  type report footing line plus 1 column 1 value 'END OF LEDGER'.
procedure division.
    open output prt.
    initiate r.
    perform varying i from 1 by 1 until i > 6
        move r-dept (i) to dept
        move r-acct (i) to acct
        move r-amt (i) to amt
        generate det
    end-perform.
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
    display 'grand total again: ' d-tot.
    stop run.
end program rptctl.
