' a FUNCTION declared with its suffix, named without it under DEFINT:
' still the FUNCTION (QBasic 1.1), the DEFtype's suffix not added
DEFINT A-Z
DECLARE FUNCTION ltsb$ (nu&)
fr& = 42
PRINT "You get $" + ltsb(fr&); ltsb$(7)
FUNCTION ltsb$ (nu&)
ltsb = LTRIM$(STR$(nu&))
END FUNCTION
