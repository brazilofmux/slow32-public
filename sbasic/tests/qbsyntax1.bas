REM QuickBASIC forms the interpreter now takes (checked against QBasic 1.1):
REM WIDTH's rows, DEFtype lists, DIM of plain variables, IF ... THEN : ...,
REM a file number without #, CLOSE lists, INPUT # and LINE INPUT # into
REM array elements
WIDTH 80, 25
DEFINT A-C, X: DEFSNG G, J
A = 2.6: X = 3.7: G = 1.5: PRINT A; X; G
DIM N%, P, Q AS STRING
N% = 2.6: Q = "s": PRINT N%; P; Q
C = 0: IF C = 0 THEN : PRINT "then": PRINT "more"
OPEN "QBSYN.TMP" FOR OUTPUT AS 1: PRINT #1, "first line": PRINT #1, "7,eight": CLOSE 1
OPEN "QBSYN.TMP" FOR INPUT AS #2: OPEN "QBSYN2.TMP" FOR OUTPUT AS 3
DIM L$(2), V(3), W$(3)
LINE INPUT #2, L$(1): INPUT #2, V(2), W$(2)
CLOSE #2, #3
KILL "QBSYN.TMP": KILL "QBSYN2.TMP"
PRINT L$(1); V(2); W$(2)
