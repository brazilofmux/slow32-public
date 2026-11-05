' an array DIMmed before its COMMON, in both programs: the same array
DIM v%(3)
COMMON v%()
v%(1) = 5
OPEN "CHTD.BAS" FOR OUTPUT AS #9
PRINT #9, "DIM w%(3)"
PRINT #9, "COMMON w%()"
PRINT #9, "PRINT w%(1); UBOUND(w%)"
PRINT #9, "KILL " + CHR$(34) + "CHTD.BAS" + CHR$(34)
PRINT #9, "SYSTEM"
CLOSE #9
CHAIN "CHTD.BAS"
