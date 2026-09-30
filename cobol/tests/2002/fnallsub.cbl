*> ALL as a subscript (X3.23a-1989 2.2; 2023 15.3): every element, the
*> rightmost ALL varying fastest; ALL mixed with ordinary and relative
*> subscripts; a table with OCCURS DEPENDING ON taken to the DEPENDING ON
*> item's current value; strings to MAX, MIN and ORD-MAX.  Before this
*> sweep only name(ALL) on a one-dimension table was taken, its elements
*> counted to the OCCURS maximum whatever the DEPENDING ON item held, and
*> MAX and MIN over strings saw the first element only.  No oracle:
*> GnuCOBOL 4.0-early-dev refuses ALL here; reviewed by hand.
identification division.
program-id. fnallsub.
data division.
working-storage section.
01 t1.
   05 v pic s9(3) occurs 4 value 0.
01 t2.
   05 row occurs 3.
      10 c pic 99 occurs 4.
01 n pic 9 value 2.
01 t3.
   05 e pic 9(3) occurs 1 to 6 depending on n.
01 g.
   05 w pic x(3) occurs 3.
01 r pic -9(5).9(4).
01 i pic 9 value 2.
procedure division.
    move 5 to v(1) move -7 to v(2) move 12 to v(3) move 3 to v(4)
    compute r = function sum(v(all))            display "sum v      " r
    compute r = function max(v(all))            display "max v      " r
    compute r = function ord-min(v(all))        display "ordmin v   " r
    perform varying i from 1 by 1 until i > 3
        move i to c(i, 1) compute c(i, 2) = i * 10 move 7 to c(i, 3) move 1 to c(i, 4)
    end-perform
    compute r = function sum(c(all, all))       display "sum c all  " r
    compute r = function sum(c(2, all))         display "sum c 2,.  " r
    compute r = function sum(c(all, 2))         display "sum c .,2  " r
    move 2 to i
    compute r = function max(c(all, i + 1))     display "max c .,i+1" r
    compute r = function ord-max(c(all, all))   display "ordmax c   " r
    compute r = function mean(c(all, 1) 100)    display "mean c .,1 " r
    move 100 to e(1) move 200 to e(2) move 300 to e(3)
    compute r = function sum(e(all))            display "sum odo 2  " r
    move 3 to n
    compute r = function sum(e(all))            display "sum odo 3  " r
    move "bob" to w(1) move "zed" to w(2) move "amy" to w(3)
    display "max w      [" function max(w(all)) "]"
    display "min w      [" function min(w(all) "b") "]"
    compute r = function ord-max(w(all))        display "ordmax w   " r
    stop run.
end program fnallsub.
