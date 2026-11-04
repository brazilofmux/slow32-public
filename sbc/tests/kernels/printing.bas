' PRINT: numbers and strings to the screen
DEFINT A-Z
FOR i = 1 TO 2500
  PRINT i; i * 2; "x";
  IF i MOD 8 = 0 THEN PRINT
NEXT
PRINT
