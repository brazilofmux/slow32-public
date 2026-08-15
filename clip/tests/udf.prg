FUNCTION PAD_ZERO
PARAMETERS cStr, nWidth
PRIVATE cTrim, nPad
cTrim = TRIM(cStr)
nPad = nWidth - LEN(cTrim)
IF nPad > 0
  RETURN REPLICATE("0", nPad) + cTrim
ENDIF
RETURN cTrim
