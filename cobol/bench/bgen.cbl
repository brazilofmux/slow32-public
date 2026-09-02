*> bgen -- write the synthetic 75-byte record file the other benches read.
identification division.
program-id. bgen.
environment division.
input-output section.
file-control.
    select out-file assign to 'bench.dat'
        organization is line sequential access is sequential.
data division.
file section.
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
01  ws-i pic 9(9) comp value 0.
procedure division.
main-procedure.
    open output out-file
    move 1234567890 to o-txn-number
    move 20260501   to o-txn-date
    move 'REFERENCE' to o-txn-reference
    move 'SOURCE'   to o-txn-source
    move 12         to o-txn-desc-id
    move 42         to o-lin-act-id
    move 'D'        to o-lin-type
    move 123.45     to o-lin-amount
    move 77         to o-lin-desc-id
    move 1          to o-com-id
    perform 56164 times
        write out-record
    end-perform
    close out-file
    goback.
end program bgen.
