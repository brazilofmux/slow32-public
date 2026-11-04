' a subscript out of range inside a loop the compiler holds the array for
' stops the program there, with what QBasic says
DEFINT A-Z
DIM a(4)
FOR i = 0 TO 9
  a(i) = i
  PRINT i;
NEXT
PRINT "not reached"
