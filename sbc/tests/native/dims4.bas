' arrays of four and five dimensions, compiled: DIM with lo TO hi and
' without, elements read and written, LBOUND and UBOUND of each
' dimension, REDIM, ERASE, a SUB given one, records, a subscript out of
' range
DECLARE SUB tally (h%())
TYPE pt
  v AS INTEGER
END TYPE
DIM h%(0 TO 3, 1 TO 2, 0 TO 1, 2 TO 3), q#(2, 2, 2, 2, 2), r(1, 1, 1, 1) AS pt
ON ERROR GOTO oops
FOR a = 0 TO 3: FOR b = 1 TO 2: FOR c = 0 TO 1: FOR d = 2 TO 3
  h%(a, b, c, d) = a * 1000 + b * 100 + c * 10 + d
NEXT: NEXT: NEXT: NEXT
PRINT h%(3, 2, 1, 3); h%(0, 1, 0, 2); h%(2, 1, 1, 2)
FOR k = 1 TO 4: PRINT LBOUND(h%, k); UBOUND(h%, k);: NEXT: PRINT
q#(2, 1, 0, 2, 1) = 1.25: q#(0, 0, 0, 0, 0) = q#(2, 1, 0, 2, 1) * 4
PRINT q#(0, 0, 0, 0, 0); UBOUND(q#, 5)
r(1, 0, 1, 0).v = 77: PRINT r(1, 0, 1, 0).v
tally h%()
x = h%(4, 1, 0, 2)
PRINT "after"
REDIM s$(1 TO 2, 3, 1, 1)
s$(2, 3, 1, 1) = "four"
PRINT s$(2, 3, 1, 1); UBOUND(s$, 2)
ERASE s$
REDIM s$(1, 1, 1, 1)
PRINT "["; s$(1, 1, 1, 1); "]"
END
oops:
PRINT "err"; ERR
RESUME NEXT
SUB tally (h%())
FOR a = LBOUND(h%, 1) TO UBOUND(h%, 1)
  t& = t& + h%(a, 2, 1, 3)
NEXT
PRINT "sum"; t&
h%(1, 1, 1, 2) = -5
PRINT h%(1, 1, 1, 2)
END SUB
