*> >>FLAG-14 (2023 7.3.15; standard-queue item 34): every option flagged
*> once at least, two turned off part way (rules 2-3).
>>FLAG-14 ALL ON
>>DEFINE Q AS 7 / 2
>>EVALUATE Q
>>WHEN 3
>>WHEN OTHER
>>END-EVALUATE
identification division.
program-id. flag14-all.
environment division.
input-output section.
file-control.
    select ixf assign to "f14-ix.dat" organization indexed access dynamic record key ix-key file status fs.
    select prt assign to "f14-prt.txt" organization line sequential.
data division.
file section.
fd ixf.
01 ix-rec.
   05 ix-key pic x(4).
   05 ix-data pic x(10).
fd prt linage 5 lines.
01 prt-rec pic x(20).
working-storage section.
01 fs pic xx.
01 ne pic zz9.99 value zero.
01 ne2 pic zz9.99 value 12.5.
01 s pic x(10) value "abcdefghij".
01 n pic 9 value 1.
procedure division.
declaratives.
err-sec section.
    use after error procedure on i-o.
err-para.
    display "declarative".
end declaratives.
main section.
    open i-o ixf.
    if fs = "04" display "04" end-if.
    if "07" = fs display "07" end-if.
    read ixf next.
    read ixf previous.
    rewrite ix-rec.
    read ixf next at end continue end-read.
    close ixf.
    open output prt.
    write prt-rec.
    write prt-rec at end-of-page continue end-write.
    close prt.
    >>TURN EC-BOUND-REF-MOD CHECKING ON
    move s(2:n) to s.
    >>REF-MOD-ZERO-LENGTH OFF
    move s(2:n) to s.
>>FLAG-14 READ-PREVIOUS OFF
    read ixf previous.
>>FLAG-14 EVALUATE I-O-STATUS-04 OFF
    if fs = "04" display "04" end-if.
    if fs = "07" display "07" end-if.
    stop run.
