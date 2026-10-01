*> A string function's argument that is reference-modified is that many
*> characters, not the item from the start position on: NUMVAL(t(p:1))
*> read t from p to its end, so "2023" scanned a digit at a time gave
*> 2023, 23, 23, 3.  The same held for NUMVAL-C, NUMVAL-F, TEST-NUMVAL,
*> REVERSE, and the arguments of the list functions.  Found by the csv2fw
*> port in majesty (2026-09-30).
identification division.
program-id. fnargrm.
data division.
working-storage section.
01  p                           pic 9(5) comp value 2.
01  q                           pic 9(5) comp value 1.
01  t                           pic x(8) value "2023".
01  x                           pic x(8).
01  n                           pic 9(5).
procedure division.
    move "[" to x  move t(p:1) to x  display "move t(p:1)       [" x "]"
    compute n = function length(t(p:1))      display "length(t(p:1))    " n
    move function upper-case(t(p:1)) to x    display "upper(t(p:1))     [" x "]"
    compute n = function numval(t(p:1))      display "numval(t(p:1))    " n
    compute n = function numval(t(2:1))      display "numval(t(2:1))    " n
    compute n = function numval(t(p:q))      display "numval(t(p:q))    " n
    compute n = function numval(t(2:q))      display "numval(t(2:q))    " n
    move "12.50" to t
    compute n = function numval(t(2:3)) * 100         display "numval(t(2:3))*100 " n
    move function reverse(t(1:3)) to x                display "reverse(t(1:3))   [" x "]"
    compute n = function numval-c(t(1:2))             display "numval-c(t(1:2))  " n
    compute n = function max(function numval(t(1:1)) function numval(t(2:1)))
                                                       display "max of digits     " n
    stop run.
