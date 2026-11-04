' INTEGER and LONG arithmetic in nested loops
DEFINT A-Z
DIM t AS LONG
FOR i = 1 TO 600
  FOR j = 1 TO 300
    k = (i * 3 + j) MOD 97
    IF k > 40 THEN t = t + k ELSE t = t - (k \ 3)
  NEXT
NEXT
PRINT t
