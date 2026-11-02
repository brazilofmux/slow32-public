REM A NEXT that overflows ends its loop; a FOR whose start overflowed,
REM resumed past, has its NEXT step by the FOR's STEP to a limit of 0
REM (QBasic 1.1's output)
ON ERROR GOTO 900
PRINT "one"
FOR B% = 32765 TO 32767
PRINT B%;
NEXT B%
PRINT "two"
B% = -3: K& = 0
FOR B% = 1D+300 TO -32768
K& = K& + 1: PRINT "b";
IF K& > 5 THEN EXIT FOR
NEXT B%
PRINT "end"
END
900 PRINT "E"; ERR: RESUME NEXT
