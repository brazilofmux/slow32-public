PRINT "start"
ON ERROR GOTO 9
X = 1 / 0
PRINT "not reached"
END
9 PRINT "handler"; ERR
Y = 1 / 0
PRINT "not reached either"
