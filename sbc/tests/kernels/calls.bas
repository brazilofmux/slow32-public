' SUB and FUNCTION calls: recursion, parameters by reference
DECLARE FUNCTION fib% (n%)
DECLARE SUB bump (v&, by%)
DEFINT A-Z
DIM t AS LONG
PRINT fib(19)
FOR i = 1 TO 20000: bump t, i MOD 7: NEXT
PRINT t
FUNCTION fib% (n%)
IF n% < 2 THEN fib% = n% ELSE fib% = fib%(n% - 1) + fib%(n% - 2)
END FUNCTION
SUB bump (v&, by%)
v& = v& + by%
END SUB
