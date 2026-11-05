REM SADD of a field, a fixed-length string: QB says Fixed-length string illegal
TYPE r
 s AS STRING * 2
END TYPE
DIM v AS r
PRINT SADD(v.s)
