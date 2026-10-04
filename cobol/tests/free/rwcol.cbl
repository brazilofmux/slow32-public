*> COL, COLS and COLUMNS, spellings of the COLUMN clause (2023 13.18.14.2
*> format 1), leading a report entry: "05 COL 1 ..." is a field at
*> column 1, not an entry named COL.  ACAS's IRS reports (cobol
*> ISSUES-124) write them under a "03 LINE n." entry.  The program
*> reads its own print file back.
*> No oracle: GnuCOBOL 4.0 refuses COL and COLS here, which the standard allows.
identification division.
program-id. rwcol.
environment division.
input-output section.
file-control.
    select prt assign to 'rwcol.prn' organization line sequential.
    select chk assign to 'rwcol.prn' organization line sequential.
data division.
file section.
fd  prt report is r.
fd  chk.
01  chk-line pic x(40).
working-storage section.
77  client  pic x(8) value 'APPLEWD'.
77  eof-f   pic 9 value 0.
report section.
rd  r
    page limit is 20
    heading 1
    first detail 4.
01  report-head-group type page heading.
    03  line 1.
        05  col  1        pic x(8)   source client.
        05  col 12        pic x(6)   value 'REPORT'.
    03  line 2.
        05  cols 3        pic x(4)   value 'COLS'.
        05  columns 9     pic x(7)   value 'COLUMNS'.
01  det type detail line plus 1.
    05  col 2 pic x(6) value 'DETAIL'.
procedure division.
    open output prt
    initiate r
    generate det
    terminate r
    close prt
    open input chk
    perform until eof-f = 1
        read chk at end move 1 to eof-f
                 not at end display '[' chk-line ']'
        end-read
    end-perform
    close chk
    stop run.
