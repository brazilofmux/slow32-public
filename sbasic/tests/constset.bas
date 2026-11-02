' A CONST is not a variable: assigning it, or counting a FOR with it,
' is an error, after it has been read and before (the compiled
' program caches what a name reads)
ON ERROR GOTO 900
CONST LIMIT = 5
CONST NAME$ = "fixed"
B = 1
PRINT LIMIT + B; NAME$
LIMIT = 6
PRINT LIMIT
NAME$ = "moved"
PRINT NAME$
FOR LIMIT = 1 TO 2: PRINT "body"; LIMIT: NEXT
FOR I = 1 TO LIMIT: PRINT I;: NEXT: PRINT
PRINT "end"; LIMIT
END
900 PRINT "error"; ERR: RESUME NEXT
