' String concatenation benchmark
s$ = ""
FOR i% = 1 TO 500
    s$ = s$ + "x"
NEXT
PRINT "Length ="; LEN(s$)
s$ = ""
FOR i% = 1 TO 200
    s$ = s$ + STR$(i%)
NEXT
PRINT "Num length ="; LEN(s$)
END
