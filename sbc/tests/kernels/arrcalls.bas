' an array used between procedure calls: each call changes what a cached
' array lookup may trust
DECLARE FUNCTION mix% (v%)
DECLARE SUB note (v%)
DEFINT A-Z
DIM SHARED tot&
DIM t(50)
FOR r = 1 TO 200
  FOR i = 0 TO 50
    t(i) = mix(t(i) + i)
    IF i MOD 10 = 0 THEN note t(i)
  NEXT
NEXT
PRINT t(7); tot&
FUNCTION mix% (v%)
mix% = (v% * 3 + 1) MOD 251
END FUNCTION
SUB note (v%)
tot& = tot& + v%
END SUB
