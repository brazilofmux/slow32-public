' arrays of a procedure, its own and passed in, with calls among the uses
DECLARE FUNCTION mix% (v%)
DECLARE SUB churn (a%(), n%)
DECLARE SUB ownarr (n%)
DEFINT A-Z
DIM t(50)
FOR r = 1 TO 100
  churn t(), 50
  ownarr 40
NEXT
PRINT t(7); t(50)
SUB churn (a(), n)
FOR i = 0 TO n
  a(i) = mix(a(i) + i)
NEXT
END SUB
SUB ownarr (n)
DIM b(n)
FOR i = 1 TO n
  b(i) = mix(b(i - 1) + i)
NEXT
s = 0
FOR i = 0 TO n
  s = s + b(i)
NEXT
IF s < 0 THEN PRINT s
END SUB
FUNCTION mix% (v%)
mix% = (v% * 3 + 1) MOD 251
END FUNCTION
