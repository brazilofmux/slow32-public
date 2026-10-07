*> EXTERNAL conformance (2023 14.8.4; standard-queue item 35): two programs
*> describing one external file and one external record alike, EC-EXTERNAL
*> checked in both, nothing raised; the FILE STATUS and RELATIVE KEY items
*> in the external record (E.2 items 12 and 24), the sub's statuses in the
*> shared item. No oracle: GnuCOBOL 4 has no EC-EXTERNAL checking.
identification division.
program-id. extconform.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extconform-rel.dat" organization relative access random relative key rk file status fs.
data division.
file section.
fd relf is external.
01 relf-rec pic x(8).
working-storage section.
01 shared is external.
   05 fs pic xx.
   05 rk pic 9(4).
procedure division.
    open output relf.
    display "open: " fs.
    move 3 to rk.
    move "record 3" to relf-rec.
    write relf-rec.
    display "write: " fs.
    close relf.
    call "extsub".
    display "after sub: fs=" fs " rk=" rk.
    stop run.
end program extconform.
identification division.
program-id. extsub.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extconform-rel.dat" organization relative access random relative key rk file status fs.
data division.
file section.
fd relf is external.
01 relf-rec pic x(8).
working-storage section.
01 shared is external.
   05 fs pic xx.
   05 rk pic 9(4).
procedure division.
    open input relf.
    move 3 to rk.
    read relf.
    display "sub read: " fs " [" relf-rec "]".
    move 9 to rk.
    read relf.
    display "sub read 9: " fs.
    close relf.
end program extsub.
