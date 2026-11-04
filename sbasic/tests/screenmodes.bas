' text in each graphics mode as QBasic keeps it on a VGA: SCREEN clears
' and homes the cursor; 40 columns in 1, 7 and 13 and 80 in the rest
' (a line of 81 wraps by them); 30 rows in 11 and 12; SCREEN 0 after a
' mode takes its columns; WIDTH's rows per mode (30 and 60 at 480 lines
' only), WIDTH 40 refused at 480 -- the values printed last
DIM r(200), e$(1)
ON ERROR GOTO caught
DATA 1,2,7,8,9,11,12,13
FOR m = 1 TO 8
  READ md
  SCREEN md
  PRINT "Qz";
  r(n) = POS(0): r(n + 1) = CSRLIN: r(n + 2) = SCREEN(1, 1): r(n + 3) = SCREEN(1, 2)
  DEF SEG = 0: r(n + 4) = PEEK(&H44A): r(n + 5) = PEEK(&H449): DEF SEG
  LOCATE 4, 1: PRINT STRING$(81, "o");
  r(n + 6) = POS(0): r(n + 7) = CSRLIN
  k = m: LOCATE 30, 1
  k = m + 10: LOCATE 31, 1
  k = m + 20: LOCATE 26, 1
  n = n + 8
NEXT
DATA 12,43,9,50,13,43,2,43,11,60,9,30,12,50,9,60,12,26,12,25
FOR i = 1 TO 10
  READ md, rows
  k = 30 + i: SCREEN md: WIDTH , rows
  r(n) = 0
  FOR t = 25 TO 61
    bad = 0: LOCATE t, 1: IF bad = 0 THEN r(n) = t
  NEXT
  n = n + 1
NEXT
k = 41: SCREEN 11: WIDTH 40
k = 42: SCREEN 12: WIDTH 40
k = 43: SCREEN 9: WIDTH 40: LOCATE 1, 40
k = 44: LOCATE 1, 41
SCREEN 13: SCREEN 0
DEF SEG = 0: r(n) = PEEK(&H44A): DEF SEG
SCREEN 12: SCREEN 0
DEF SEG = 0: r(n + 1) = PEEK(&H44A): DEF SEG
n = n + 2
SCREEN 0: WIDTH 80
FOR i = 0 TO n - 1: PRINT r(i);: IF i MOD 8 = 7 THEN PRINT
NEXT
PRINT
PRINT e$(0)
END
caught:
IF k > 30 AND k < 41 THEN bad = 1 ELSE e$(0) = e$(0) + STR$(k) + ":" + MID$(STR$(ERR), 2)
RESUME NEXT
