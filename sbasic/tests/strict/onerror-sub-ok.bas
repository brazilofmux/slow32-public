' ON ERROR GOTO in a SUB names the main program's handler: QBasic loads it
DECLARE SUB risky ()
risky
PRINT "back"
END
handler:
PRINT "caught"; ERR
RESUME NEXT
SUB risky
  ON ERROR GOTO handler
  ERROR 5
END SUB
