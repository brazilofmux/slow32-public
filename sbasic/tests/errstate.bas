' what ERR and ERL say, as QBasic keeps them: ERR is 0 after any RESUME
' (NEXT, the statement again, a label), ERL stays; ON ERROR GOTO another
' label inside the handler changes the handler, and this one still
' RESUMEs; ERL in a procedure is its own last line number, 0 if none
DECLARE FUNCTION Dbl% (V%)
DECLARE SUB Lined ()
10 ON ERROR GOTO 900
20 x = 1 / 0
30 PRINT "a"; ERR; ERL
40 ERROR 33
50 PRINT "b"; ERR
60 ON ERROR GOTO 910
70 y = SQR(-1)
80 PRINT "c"; ERR
90 ON ERROR GOTO 920
100 y = LOG(0)
110 PRINT "never"
120 PRINT "d"; ERR; ERL
130 ON ERROR GOTO 900
140 PRINT Dbl%(30000); "fn"
150 Lined
160 PRINT "e"; ERR; ERL
170 END
900 PRINT "h"; ERR; ERL: RESUME NEXT
910 PRINT "i"; ERR: ON ERROR GOTO 900: RESUME NEXT
920 PRINT "j"; ERR: RESUME 120
FUNCTION Dbl% (V%)
    Dbl% = V% * 2
END FUNCTION
SUB Lined
500 a = 1
510 b = 1 / 0
    c = SQR(-4)
END SUB
