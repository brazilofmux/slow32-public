identification division.
program-id. divround18.
*> ROUNDED into an 18-digit receiver needs the 19th digit of the
*> quotient; the narrow division stops at 18, and the last digit was
*> truncated rather than rounded (found by the differential generator,
*> tests/gen).  73844192123.9531 / 0.561 = 131629575978.5260249554...
data division.
working-storage section.
01 dvd  pic s9(11)v9(4) usage packed-decimal.
01 dvs  pic sv9(3) usage binary.
01 q    pic 9(12)v9(6) usage packed-decimal.
01 q17  pic 9(12)v9(5) usage packed-decimal.
procedure division.
    move 73844192123.9531 to dvd  move 0.561 to dvs
    divide dvd by dvs giving q rounded
    display "giving  " q
    divide dvs into dvd giving q rounded
    display "into    " q
    move dvd to q
    divide dvs into q rounded
    display "format1 " q
    compute q rounded = dvd / dvs
    display "compute " q
    divide dvd by dvs giving q
    display "trunc   " q
*> 17 digits: the 18th digit decides (.52602|4955 -> .52602)
    divide dvd by dvs giving q17 rounded
    display "17      " q17
    stop run.
