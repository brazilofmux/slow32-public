*> EC-EXTERNAL-FORMAT-CONFLICT (2023 14.8.4.3; 13.18.22.4 rule 6): the sub
*> describes the external record two bytes longer; its declarative takes
*> the fatal condition, then the run unit ends (exit 3). No oracle.
identification division.
program-id. extformat.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extformat-rel.dat" organization relative access random relative key rk file status fs.
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
end program extformat.
identification division.
program-id. extsub.
>>TURN EC-EXTERNAL CHECKING ON
environment division.
input-output section.
file-control.
    select relf assign to "extformat-rel.dat" organization relative access random relative key rk file status fs.
data division.
file section.
fd relf is external.
01 relf-rec pic x(8).
working-storage section.
01 shared is external.
   05 fs pic xx.
   05 rk pic 9(6).
procedure division.
declaratives.
ext-sec section.
    use after exception condition ec-external-format-conflict.
ext-para.
    display "declarative: " function exception-status.
end declaratives.
main section.
    open input relf.
    move 3 to rk.
    read relf.
    display "sub read: " fs " [" relf-rec "]".
    move 9 to rk.
    read relf.
    display "sub read 9: " fs.
    close relf.
end program extsub.
