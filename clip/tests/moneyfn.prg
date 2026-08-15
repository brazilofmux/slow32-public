FUNCTION FMT_MONEY
PARAMETERS nAmount, nWidth
PRIVATE cNum, cSign, nDot, cInt, cDec, nLen, nPos

cNum = LTRIM(STR(nAmount, 20, 2))
cSign = ""

IF LEFT(cNum, 1) = "-"
  cSign = "-"
  cNum = SUBSTR(cNum, 2)
ENDIF

nDot = AT(".", cNum)
IF nDot > 0
  cInt = LEFT(cNum, nDot - 1)
  cDec = SUBSTR(cNum, nDot)
ELSE
  cInt = cNum
  cDec = ".00"
ENDIF

nLen = LEN(cInt)
nPos = nLen - 3
DO WHILE nPos > 0
  cInt = LEFT(cInt, nPos) + "," + SUBSTR(cInt, nPos + 1)
  nPos = nPos - 3
ENDDO

cNum = cSign + cInt + cDec
RETURN SPACE(nWidth - LEN(cNum)) + cNum
