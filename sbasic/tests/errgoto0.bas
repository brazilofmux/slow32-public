' ON ERROR GOTO 0 in the handler: the program stops, with the error
' being handled (QBasic)
ON ERROR GOTO h
PRINT "one"
x = SQR(-1)
PRINT "not"; ERR
END
h: PRINT "in"; ERR
ON ERROR GOTO 0
PRINT "not either"
RESUME NEXT
