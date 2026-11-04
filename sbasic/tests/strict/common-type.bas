REM a COMMON SHARED array typed AS INTEGER, DIMmed AS LONG: QB says Duplicate definition
COMMON SHARED cnt() AS INTEGER
DIM cnt(2) AS LONG
PRINT cnt(1)
