*> kstring -- STRING with a pointer, UNSTRING on a delimiter, INSPECT
*> TALLYING and CONVERTING, over a line that changes every iteration.
identification division.
program-id. kstring.
data division.
working-storage section.
01  n        pic 9(9) comp value 500000.
01  i        pic 9(9) comp.
01  cid       pic 9(8).
01  ln     pic x(60).
01  p        pic 99 comp.
01  f1       pic x(10).
01  f2       pic 9(8).
01  f3       pic x(20).
01  t        pic 9(9) comp value 0.
01  tot      pic 9(15) comp-3 value 0.
procedure division.
    perform varying i from 1 by 1 until i > n
        move i to cid
        move spaces to ln
        move 1 to p
        string "cust-" cid "-alpha street-" cid(5:4) delimited by size
            into ln with pointer p
        unstring ln delimited by "-" into f1 f2 f3
        inspect ln tallying t for all "a"
        inspect ln converting "abcdefghijklmnopqrstuvwxyz"
            to "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        add f2 to tot
        if ln(1:4) = "CUST" add p to t end-if
    end-perform
    display "kstring " t " " tot " " ln
    stop run.
