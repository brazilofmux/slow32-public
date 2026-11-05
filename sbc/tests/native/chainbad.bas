' COMMON's positions must have the same types
COMMON a%, b$
a% = 1: b$ = "x"
OPEN "CHTB.TMP" FOR OUTPUT AS #9
PRINT #9, "COMMON c$, d%"
PRINT #9, "PRINT 1"
PRINT #9, "SYSTEM"
CLOSE #9
PRINT "before"
CHAIN "CHTB.TMP"
PRINT "not reached"
