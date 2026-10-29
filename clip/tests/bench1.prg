* A loop-heavy program for timing: arithmetic, strings, a UDF
nSum = 0
cStr = ""
FOR i = 1 TO 2000
  nSum = nSum + MOD(i * 7, 13) + SQUARE(i)
  IF MOD(i, 100) = 0
    cStr = cStr + STR(i, 5)
  ENDIF
NEXT
j = 0
DO WHILE j < 3000
  j = j + 1
  IF j % 2 = 0 .AND. j > 10
    nSum = nSum - 1
  ENDIF
ENDDO
? nSum
? cStr
RETURN

FUNCTION SQUARE
PARAMETERS n
RETURN n * n
