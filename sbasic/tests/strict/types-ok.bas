REM forms QB loads: IF c GOTO n, LEN of a numeric variable, a FUNCTION by its bare name,
REM strings compared, string + string, a string array's element in a SUB through SHARED
DECLARE FUNCTION twice% (v%)
DECLARE SUB show ()
DIM SHARED tags$(2)
N% = 3
PRINT LEN(N%); twice(N%); "ab" < "b"; "a" + "b"
tags$(1) = "z"
show
IF N% = 3 GOTO 10
PRINT "not"
10 PRINT "done"
FUNCTION twice% (v%)
twice% = v% * 2
END FUNCTION
SUB show
PRINT tags$(1) + "!"
END SUB
