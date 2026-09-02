*> b2 -- READ, one group MOVE of the whole record, WRITE.
identification division.
program-id. b2.
environment division.
input-output section.
file-control.
    select in-file assign to 'bench.dat'
        organization is line sequential access is sequential.
    select out-file assign to 'b2out.dat'
        organization is line sequential access is sequential.
data division.
file section.
fd  in-file.
01  in-record.
    05  d-txn-number    pic 9(10).
    05  d-txn-date      pic 9(8).
    05  d-txn-reference pic x(10).
    05  d-txn-source    pic x(8).
    05  d-txn-desc-id   pic 9(10).
    05  d-lin-act-id    pic 9(5).
    05  d-lin-type      pic x.
    05  d-lin-amount    pic 9(9)v99.
    05  d-lin-desc-id   pic 9(10).
    05  d-com-id        pic 9(2).
fd  out-file.
01  out-record.
    05  o-txn-number    pic 9(10).
    05  o-txn-date      pic 9(8).
    05  o-txn-reference pic x(10).
    05  o-txn-source    pic x(8).
    05  o-txn-desc-id   pic 9(10).
    05  o-lin-act-id    pic 9(5).
    05  o-lin-type      pic x.
    05  o-lin-amount    pic 9(9)v99.
    05  o-lin-desc-id   pic 9(10).
    05  o-com-id        pic 9(2).

working-storage section.
01  ws-eof pic x value 'N'.
procedure division.
main-procedure.
    open input in-file
    open output out-file
    perform until ws-eof = 'Y'
        read in-file
            at end move 'Y' to ws-eof
            not at end
                move in-record to out-record
                write out-record
        end-read
    end-perform
    close in-file out-file
    goback.
end program b2.
