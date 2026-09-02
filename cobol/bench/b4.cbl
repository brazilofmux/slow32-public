*> b4 -- the seven NUMERIC moves of b3 only (the five PIC X ones dropped).
identification division.
program-id. b4.
data division.
working-storage section.
01  ws-i pic 9(9) comp value 0.
01  src-rec.
    05  d-txn-number    pic 9(10)   value 1234567890.
    05  d-txn-date      pic 9(8)    value 20260501.
    05  d-txn-reference pic x(10)   value 'REFERENCE'.
    05  d-txn-source    pic x(8)    value 'SOURCE'.
    05  d-txn-desc-id   pic 9(10)   value 12.
    05  d-lin-act-id    pic 9(5)    value 42.
    05  d-lin-type      pic x       value 'D'.
    05  d-lin-amount    pic 9(9)v99 value 123.45.
    05  d-lin-desc-id   pic 9(10)   value 77.
    05  d-com-id        pic 9(2)    value 1.
01  act-rec.
    05  a-number        pic 9(10)   value 1000000001.
    05  a-type          pic x       value 'E'.
    05  a-name          pic x(40)   value 'ACCOUNT NAME'.
01  out-rec.
    05  o-txn-number    pic 9(10).
    05  o-txn-date      pic 9(8).
    05  o-txn-reference pic x(10).
    05  o-txn-source    pic x(8).
    05  o-txn-desc-id   pic 9(10).
    05  o-lin-type      pic x.
    05  o-lin-amount    pic 9(9)v99.
    05  o-lin-desc-id   pic 9(10).
    05  o-com-id        pic 9(2).
    05  o-a-number      pic 9(10).
    05  o-a-type        pic x.
    05  o-a-name        pic x(40).
procedure division.
main-procedure.
    perform varying ws-i from 1 by 1 until ws-i > 56164
        move d-txn-number    to o-txn-number
        move d-txn-date      to o-txn-date
        move d-txn-desc-id   to o-txn-desc-id
        move d-lin-amount    to o-lin-amount
        move d-lin-desc-id   to o-lin-desc-id
        move d-com-id        to o-com-id
        move a-number        to o-a-number
    end-perform
    goback.
end program b4.
