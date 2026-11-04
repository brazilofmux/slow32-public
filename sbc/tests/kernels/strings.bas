' strings: building, MID$, comparison, ASC and CHR$
DEFINT A-Z
FOR r = 1 TO 150
  s$ = ""
  FOR i = 1 TO 120
    s$ = s$ + CHR$(65 + (i * r) MOD 26)
  NEXT
  n = 0
  FOR i = 1 TO LEN(s$)
    IF MID$(s$, i, 1) = "E" THEN n = n + 1
    h = (h * 3 + ASC(MID$(s$, i, 1))) MOD 1009
  NEXT
  IF LEFT$(s$, 2) < RIGHT$(s$, 2) THEN m = m + n
NEXT
PRINT m; h
