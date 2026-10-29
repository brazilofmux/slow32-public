* Statements: FUNCTION with formal parameters, PCOUNT()
? Two(1, 2), Two(5)

FUNCTION Two(a, b)
IF PCOUNT() < 2
   RETURN -1
ENDIF
RETURN a + b
