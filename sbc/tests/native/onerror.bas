10 ' ON ERROR and RESUME in s32-bc
20 ON ERROR GOTO 1000
30 A% = 32767: A% = A% + 1: PRINT "after overflow"; A%
40 X = 0: Y = 1 / X: PRINT "after div"; Y
50 DIM Q(3): Q(5) = 1: PRINT "after subscript"
60 S$ = "x": ERROR 13: PRINT "after mismatch"
70 ERROR 77: PRINT "after ERROR 77"
80 CALL Bad(3): PRINT "after CALL"
90 IF 1 / X > 0 THEN PRINT "then part" ELSE PRINT "else part"
100 FOR I% = 32766 TO 32767: PRINT "loop"; I%: NEXT I%: PRINT "past loop"
110 N = 0: WHILE 1 / N > 5: PRINT "in while": N = 1: WEND: PRINT "past while"
120 SELECT CASE 1 / X
CASE 1: PRINT "one"
CASE ELSE: PRINT "else"
END SELECT
125 PRINT "past select"
130 GOSUB 500: PRINT "back from gosub"
140 R = 0: Z = 10 / R: PRINT "retried"; Z
150 ERROR 200
160 PRINT "skipped by label"
170 PRINT "at 170": PRINT Twice%(20000); "fn"
180 PRINT "done"; ERR; ERL
190 END
500 PRINT "in gosub": W = 1 / X: PRINT "gosub after error": RETURN
1000 PRINT "E"; ERR; "at"; ERL
1010 IF ERR = 11 AND ERL = 140 THEN R = 2: RESUME
1020 IF ERR = 200 THEN RESUME 170
1030 RESUME NEXT
SUB Bad (N)
    DIM L(N)
    L(N + 1) = 1
    PRINT "not in Bad"
END SUB
FUNCTION Twice% (V%)
    Twice% = V% * 2
END FUNCTION
