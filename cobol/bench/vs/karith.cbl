*> karith -- decimal arithmetic: COMPUTE ROUNDED, ADD, DIVIDE ...
*> REMAINDER and MULTIPLY on COMP-3 and signed DISPLAY items, every value
*> depending on the loop index, the totals printed as a checksum.
identification division.
program-id. karith.
data division.
working-storage section.
01  n        pic 9(9) comp value 2000000.
01  i        pic 9(9) comp.
01  a        pic s9(7)v99   comp-3.
01  b        pic s9(9)v99.
01  c        pic s9(11)v9(4) comp-3 value 0.
01  q        pic s9(9) comp.
01  r        pic s9(9) comp.
01  tot      pic s9(15)v99  comp-3 value 0.
procedure division.
    perform varying i from 1 by 1 until i > n
        compute a rounded = i * 3.25 / 8
        compute b rounded = a * 1.075 - i * 0.02
        add a b to tot
        divide i by 97 giving q remainder r
        add r to tot
        multiply 1.01 by b rounded
        compute c = c + b * 0.0001
    end-perform
    display "karith " tot " " c
    stop run.
