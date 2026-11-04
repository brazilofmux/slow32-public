REM a record array in COMMON SHARED, its DIM without AS: QB says AS clause required
TYPE spot
  x AS INTEGER
END TYPE
COMMON SHARED pts() AS spot
DIM pts(4)
pts(1).x = 3
PRINT pts(1).x
