' SINGLE and DOUBLE arithmetic: an integral and a series
DIM a AS SINGLE, b AS DOUBLE, x AS DOUBLE
FOR i% = 1 TO 40000
  x = i% / 40000#
  b = b + x * x / (1# + x)
  a = a + 1 / i%
NEXT
PRINT a; b
