REM a REDIM of an array DIM made with constant bounds: QB says Array already dimensioned
DIM k(5)
ERASE k
REDIM k(6)
PRINT UBOUND(k)
