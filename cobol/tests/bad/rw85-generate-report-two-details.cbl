identification division.
program-id. rwp.
environment division.
input-output section.
file-control.
    select rptf assign to "rwp.out" organization line sequential.
data division.
file section.
fd rptf report is rp.
working-storage section.
01 k pic 99 value 1.
01 dep pic x(3) value "aaa".
01 amt pic 9(3) value 5.
01 sgn pic s9(3) value -5.
report section.
rd rp
    control is dep
    page limit 20 heading 1 first detail 4 last detail 15 footing 18.
01 type page heading.
   05 line 1.
      10 column 1 pic x(5) value "HEAD".
01 dl type detail.
   05 line plus 1.
      10 column 1 pic x(3) source dep.
      10 column 10 pic zz9 source amt.
01 dl2 type detail.
   05 line plus 1.
      10 column 1 pic x value "d".
01 cfd type control footing dep.
   05 line plus 1.
      10 column 10 pic zzz9 sum amt.
01 type page footing.
   05 line 19.
      10 column 1 pic x(4) value "FOOT".
procedure division.
    open output rptf
    initiate rp
    generate rp
    terminate rp
    close rptf
    stop run.
