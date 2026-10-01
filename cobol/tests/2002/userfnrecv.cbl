identification division.
function-id. idx.
*> returns its argument: a subscript that depends on an item's value now
data division.
linkage section.
01  x        pic 9(2).
01  r        pic 9(2).
procedure division using x returning r.
    move x to r
    goback.
end function idx.

identification division.
program-id. userfnrecv.
*> A receiving item is identified where its statement's rules say, not
*> at the statement's beginning: a MOVE's receiver immediately before the
*> move to it (2023 14.9.25.4), an arithmetic statement's as each is
*> accessed (14.7.7 rule 4b), a DIVIDE's dividend as each is determined
*> and its REMAINDER after the quotient is stored (14.9.12.4), READ
*> INTO's after the record is read (14.9.30.4).  So a function in a
*> receiver's subscript sees what the statement has stored so far:
*> MOVE 2 TO N EL(IDX(N)) moves to EL(2) (cobol ISSUES-121).
environment division.
configuration section.
repository.
    function idx.
input-output section.
file-control.
    select f assign to "userfnrecv.dat" organization is sequential.
data division.
file section.
fd  f.
01  f-rec.
    05  f-n  pic 9(2).
    05  f-t  pic x(3).
working-storage section.
01  n        pic 9(2) value 1.
01  tb.
    05  el   pic 9(3) occurs 5 value 0.
01  tx.
    05  ex   pic x(5) occurs 5 value spaces.
01  q        pic 9(3).
01  rm       pic 9(3).
procedure division.
main.
    move 1 to n
    move 2 to n el(function idx(n))
    display "move: " el(1) " " el(2) " " el(3)
    move 1 to n move zeros to tb
    add 2 to n el(function idx(n))
    display "add: " el(1) " " el(2) " " el(3)
    move 1 to n move zeros to tb
    compute n el(function idx(n)) = 4
    display "compute: " el(1) " " el(2) " " el(3) " " el(4)
    move 1 to n move zeros to tb
    move 8 to el(1) el(2) el(3)
    divide 2 into n el(function idx(n) + 1)
    display "divide into: " n " " el(1) " " el(2) " " el(3)
    move 1 to n move zeros to tb
    divide 7 by 2 giving n remainder el(function idx(n))
    display "remainder: " n " " el(1) " " el(2) " " el(3)
    open output f
    move 3 to f-n move "abc" to f-t write f-rec
    close f
    move 0 to f-n
    open input f
    read f into ex(function idx(f-n)) at end display "no record" end-read
    close f
    display "read into: [" ex(1) "] [" ex(3) "]"
    stop run.
end program userfnrecv.
