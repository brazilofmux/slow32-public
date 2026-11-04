' LOCATE with arguments left out, WIDTH's rows, CLOSE lists, files and
' LINE INPUT into array elements, in s32-bc
CLS: PRINT "ab";: LOCATE , 10: PRINT "X";: LOCATE 5: PRINT "Y": PRINT CSRLIN; POS(0)
LOCATE 2, 3, 1, 0, 7: PRINT "Z"
WIDTH 80, 25
OPEN "BCSYN.TMP" FOR OUTPUT AS 1: PRINT #1, "first": PRINT #1, "7,eight": CLOSE 1
OPEN "BCSYN.TMP" FOR INPUT AS #2: OPEN "BCSYN2.TMP" FOR OUTPUT AS 3
DIM L$(2), V(3), W$(3), K$(1)
LINE INPUT #2, L$(1): INPUT #2, V(2), W$(2)
CLOSE #2, #3
KILL "BCSYN.TMP": KILL "BCSYN2.TMP"
PRINT L$(1); V(2); W$(2)
LINE INPUT "? "; K$(1): PRINT "["; K$(1); "]"
DEFINT A-C, X: DEFSNG G
DIM N%, P, Q AS STRING
A = 2.6: N% = 3.4: Q = "q": IF A = 3 THEN : PRINT A; N%; P; Q
