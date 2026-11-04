' RETURN without GOSUB is an error like any other, trapped; name (x)
' with no CALL passes x by value (another type is no mismatch, and the
' variable keeps its value), CALL name(x) by reference
DECLARE SUB dbl (a%)
ON ERROR GOTO caught
n% = 4: m! = 1.5
dbl (n%)
PRINT n%
dbl n%
PRINT n%
CALL dbl(n%)
PRINT n%
dbl (m!)
PRINT m!
RETURN
PRINT "after"; ERR
END
caught:
PRINT "E"; ERR
RESUME NEXT
SUB dbl (a%)
a% = a% * 2
END SUB
