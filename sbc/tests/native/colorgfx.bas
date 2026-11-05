' COLOR in graphics, mode by mode; what QBasic 1.1 prints
ON ERROR GOTO h
DEFINT A-Z
FOR t = 1 TO 15
    READ m, f, b
    SCREEN m
    PRINT "m"; m; f; b;
    IF f >= 0 AND b >= 0 THEN COLOR f, b
    IF f >= 0 AND b < 0 THEN COLOR f
    IF f = -1 AND b = -1 THEN COLOR
    IF f = -2 THEN COLOR 300
    PSET (1, 1)
    LINE (3, 1)-(4, 1)
    CIRCLE (10, 10), 3
    a = POINT(1, 1): c = POINT(3, 1): d = POINT(13, 10)
    SCREEN 0
    PRINT ":"; a; c; d
NEXT
PRINT "done"
SYSTEM
h: PRINT "E"; ERR; : RESUME NEXT
DATA 13,9,-1, 13,-2,-1, 13,9,3, 13,255,-1, 7,9,-1, 7,9,2, 7,20,-1, 12,9,-1, 12,9,2, 9,9,2
DATA 1,2,-1, 1,2,1, 2,1,-1, 11,1,-1, 8,3,-1
