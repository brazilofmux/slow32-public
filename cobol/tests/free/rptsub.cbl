identification division.
program-id. rptsub.
*> Report Writer: a SOURCE that is subscripted, or reference-modified,
*> or both -- the reference is parsed where every other one is, so a
*> detail line can print straight out of a table (gl008's tax lines).
environment division.
input-output section.
file-control.
    select prt assign to 'tmp/rptsub.prn'
        organization is line sequential.
    select back assign to 'tmp/rptsub.prn'
        organization is line sequential.
data division.
file section.
fd  prt report is tax-rep.
fd  back.
01  back-line         pic x(40).
working-storage section.
01  n                pic 99 value 0.
01  eof              pic x value 'n'.
01  tax-entries      pic 999 value 0.
01  tax-index        pic 999 value 0.
01  taxes.
    05  tax occurs 0 to 10 times depending on tax-entries.
        10  tax-code     pic x(4).
        10  tax-rate     pic 9v99.
        10  tax-total    pic s9(5)v99.
01  names.
    05  nm-all pic x(12) value 'GST PST HST '.
    05  nm redefines nm-all pic x(4) occurs 3 times.
report section.
rd  tax-rep
    page limit is 20 lines
    heading 1 first detail 3 last detail 18.
01  type page heading.
    02  line 1.
        05  column 1 value 'TAX SUMMARY'.
01  tax-line type detail.
    02  line plus 1.
        05  column 1   pic x(4)          source is tax-code(tax-index).
        05  column 7   pic 9.99          source is tax-rate(tax-index).
        05  column 13  pic ---,--9.99    source is tax-total(tax-index).
        05  column 25  pic x(3)          source is nm(tax-index)(1:3).
        05  column 30  pic x(2)          source is tax-code(tax-index)(2:2).
procedure division.
main.
    move 3 to tax-entries.
    move 'gst1' to tax-code(1). move 0.05 to tax-rate(1). move 123.45 to tax-total(1).
    move 'pst2' to tax-code(2). move 0.07 to tax-rate(2). move -20.5 to tax-total(2).
    move 'hst3' to tax-code(3). move 0.13 to tax-rate(3). move 0 to tax-total(3).
    open output prt.
    initiate tax-rep.
    perform varying tax-index from 1 by 1 until tax-index > tax-entries
        generate tax-line
    end-perform.
    terminate tax-rep.
    close prt.
    open input back.
    perform until eof = 'y'
        read back
            at end move 'y' to eof
            not at end
                add 1 to n
                display n ' [' back-line ']'
        end-read
    end-perform.
    close back.
    stop run.
