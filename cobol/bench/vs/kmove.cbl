*> kmove -- building records by MOVE: numeric to DISPLAY, alphanumeric
*> from a table by computed subscript, reference modification, group
*> MOVE; every source depends on the loop index.
identification division.
program-id. kmove.
data division.
working-storage section.
01  n        pic 9(9) comp value 3000000.
01  i        pic 9(9) comp.
01  k        pic 9(4) comp.
01  names.
    05  nm   pic x(20) occurs 100.
01  rec.
    05  r-num    pic 9(10).
    05  r-name   pic x(20).
    05  r-amt    pic s9(7)v99.
    05  r-code   pic x(4).
    05  r-flag   pic x.
01  out-rec      pic x(44).
01  tot      pic s9(15)v99 comp-3 value 0.
01  cnt      pic 9(9) comp value 0.
procedure division.
    perform varying k from 1 by 1 until k > 100
        move k to r-num
        string "NAME-" r-num(7:4) " STREET" delimited by size into nm(k)
    end-perform
    perform varying i from 1 by 1 until i > n
        compute k = function mod(i, 100) + 1
        move i to r-num
        move nm(k) to r-name
        compute r-amt = i / 100
        move r-num(7:4) to r-code
        move r-name(6:1) to r-flag
        move rec to out-rec
        add r-amt to tot
        if out-rec(11:1) = "N" and r-code(4:1) = "0" add 1 to cnt end-if
    end-perform
    display "kmove " tot " " cnt " " out-rec
    stop run.
