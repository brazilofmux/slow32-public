*> EC-EXTERNAL-DATA-MISMATCH (2023 14.8.4.2; E.2 item 12): the sub's FILE
*> STATUS for the external file is its own item, not the shared one; fatal
*> at the sub's entry (exit 3). No oracle.
identification division.
program-id. extdatamis.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extdatamis-rel.dat" organization relative access random relative key rk file status fs.
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
end program extdatamis.
identification division.
program-id. extsub.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extdatamis-rel.dat" organization relative access random relative key rk file status fs.
data division.
file section.
fd relf is external.
01 relf-rec pic x(8).
working-storage section.
01 fs pic xx.
01 shared is external.
   05 fsx pic xx.
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
