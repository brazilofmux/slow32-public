' an array COMMON brought, DIMmed after its COMMON: a second DIM
COMMON v%()
DIM v%(3)
OPEN "CHTC.TMP" FOR OUTPUT AS #9
PRINT #9, "COMMON w%()"
PRINT #9, "PRINT " + CHR$(34) + "here" + CHR$(34)
PRINT #9, "DIM w%(3)"
PRINT #9, "PRINT " + CHR$(34) + "no" + CHR$(34)
CLOSE #9
CHAIN "CHTC.TMP"
