' a two-dimensional DOUBLE array: matrix multiplication
DEFINT I-K
CONST M = 70
DIM a#(M, M), b#(M, M), c#(M, M)
FOR i = 1 TO M: FOR j = 1 TO M
  a#(i, j) = i + j: b#(i, j) = i - j
NEXT: NEXT
FOR i = 1 TO M: FOR j = 1 TO M
  s# = 0
  FOR k = 1 TO M: s# = s# + a#(i, k) * b#(k, j): NEXT
  c#(i, j) = s#
NEXT: NEXT
PRINT c#(3, 5); c#(M, M)
