REM DIM x AS type is a declaration: one in a loop does not reset x, nor
REM a record's fields (QBasic 1.1: 6, 6)
TYPE Tally
    N AS INTEGER
END TYPE
FOR I = 1 TO 3
    DIM X AS INTEGER: X = X + I
    DIM T AS Tally: T.N = T.N + I
NEXT
PRINT X; T.N
