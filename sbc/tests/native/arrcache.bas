' what compiled code caches of an array between uses must not outlive it:
' ERASE and REDIM in a SUB between uses in the main program, the same name
' a local array in a SUB and the main program's, recursion with an array
' of each activation, an array passed down, CLEAR -- the trail printed
DECLARE SUB shrink ()
DECLARE SUB grow ()
DECLARE SUB wipe ()
DECLARE SUB own ()
DECLARE SUB deep (n%)
DECLARE SUB fill (a%(), v%)
DECLARE FUNCTION peekit% (i%)
DEFINT A-Z
n = 3
REDIM SHARED m(n)
DIM SHARED k(4)
DIM t(2)
m(2) = 11: k(1) = 5
PRINT m(2); peekit(2)
grow
PRINT UBOUND(m); m(2); m(9)
m(9) = 12
PRINT peekit(9)
shrink
PRINT UBOUND(m); m(1)
m(1) = 13: PRINT peekit(1)
wipe
REDIM m(1 TO 2)
m(2) = 14: PRINT LBOUND(m); m(2); peekit(2)
own
PRINT t(1)
deep 3
fill t(), 21
PRINT t(2)
fill k(), 22
PRINT k(4)
FOR i = 1 TO 2
  m(i) = i * 100
  grow
  PRINT UBOUND(m); m(i);
  REDIM m(1 TO 2)
NEXT
PRINT
CLEAR
PRINT k(1)
REDIM m(2)
m(2) = 15: PRINT m(2)
END
SUB shrink
REDIM m(1)
END SUB
SUB grow
REDIM m(9)
END SUB
SUB wipe
ERASE m
END SUB
SUB own
DIM t(1)
t(1) = 78
PRINT t(1); UBOUND(t)
END SUB
SUB deep (n)
DIM v(n)
v(n) = n * 10
IF n > 0 THEN deep n - 1
PRINT v(n); UBOUND(v);
END SUB
SUB fill (a(), v)
FOR i = LBOUND(a) TO UBOUND(a): a(i) = v: NEXT
END SUB
FUNCTION peekit (i)
peekit = m(i)
END FUNCTION
