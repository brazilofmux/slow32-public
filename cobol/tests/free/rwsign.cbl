*> Report Writer entries (X3.23-1985 XIII): SIGN LEADING/TRAILING
*> SEPARATE in a report group (3.17; refused as "unexpected 'sign'"
*> before the sweep of 2026-09-30), SUM followed by COLUMN (the SUM
*> operands ran into the next clause), and an entry with SUM and no
*> COLUMN: a counter that is not presented (3.11.4 rule 1) but still
*> sums into the FINAL footing.  This compiler presented it at the next
*> column; GnuCOBOL presents it at column 1 (.oracle-expected;
*> docs/oracles.md).
identification division.
program-id. rwsign.
environment division.
input-output section.
file-control.
    select rptf assign to "tmp/rwsign.prn" organization line sequential.
    select rin assign to "tmp/rwsign.prn" organization line sequential file status rs.
data division.
file section.
fd rptf report is rp.
fd rin.
01 rline pic x(30).
working-storage section.
01 dep pic x(3) value "aaa".
01 amt pic s9(3) value -5.
01 i pic 9.
01 rs pic xx.
report section.
rd rp
    control is final dep
    page limit 20 heading 1 first detail 3 last detail 15 footing 18.
01 type page heading.
   05 line 1.
      10 column 1 pic x(5) value "HEAD".
01 dl type detail.
   05 line plus 1.
      10 column 1 pic x(3) source dep group indicate.
      10 column 6 pic s999 sign leading separate source amt.
      10 column 12 pic s999 sign trailing separate character source amt.
01 cfd type control footing dep.
   05 line plus 1.
      10 hidden-total pic s9(5) sum amt.
      10 pic s9(5) sum amt column 20.
01 type control footing final.
   05 line plus 2.
      10 column 1 pic x(6) value "total:".
      10 column 10 pic -(5)9 sum hidden-total.
01 type page footing.
   05 line 19.
      10 column 1 pic x(4) value "FOOT".
procedure division.
    open output rptf
    initiate rp
    perform varying i from 1 by 1 until i > 3
        generate dl
        subtract 7 from amt
    end-perform
    move "bbb" to dep
    generate dl
    terminate rp
    close rptf
    open input rin
    read rin
    perform until rs not = "00"
        display "[" rline "]"
        read rin
    end-perform
    close rin
    stop run.
