' an array of records with a fixed-length string field: QBasic loads it
TYPE cell
  label AS STRING * 6
  w AS LONG
END TYPE
DIM grid(1 TO 4) AS cell
FOR i = 1 TO 4
  grid(i).w = i * 10
  grid(i).label = "c" + LTRIM$(STR$(i))
NEXT
SWAP grid(1), grid(4)
PRINT grid(1).label; grid(1).w
