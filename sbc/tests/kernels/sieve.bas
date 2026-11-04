' an INTEGER array: the sieve, twenty times
DEFINT A-Z
CONST N = 6000
DIM f(N)
FOR p = 1 TO 20
  c = 0
  FOR i = 2 TO N: f(i) = 1: NEXT
  FOR i = 2 TO N
    IF f(i) THEN
      c = c + 1
      FOR j = i + i TO N STEP i: f(j) = 0: NEXT
    END IF
  NEXT
NEXT
PRINT c
