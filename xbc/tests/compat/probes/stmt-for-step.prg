* Statements: FOR with a negative STEP, EXIT and LOOP inside
LOCAL i, s := ""
FOR i := 10 TO 1 STEP -3
   s += STR(i, 3)
NEXT
? s
FOR i := 1 TO 10
   IF i % 2 == 0
      LOOP
   ENDIF
   IF i > 7
      EXIT
   ENDIF
   ?? i
NEXT
?
