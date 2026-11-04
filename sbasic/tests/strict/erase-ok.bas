' a SUB's array used, then ERASEd: QBasic loads it
trail 2
SUB trail (n)
DIM marks(8) AS INTEGER
FOR k = 1 TO n
  marks(k) = k: PRINT marks(k)
NEXT
ERASE marks
END SUB
