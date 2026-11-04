' RUN, compiled: the program again, everything cleared -- variables, a
' SUB's STATIC, a DIM SHARED one, arrays, the DATA read from the first,
' files closed -- from a GOSUB's routine and from inside a SUB, with an
' ON ERROR in force (s32-bc's statements then bcm_body's); the passes
' counted in a file (QBasic 1.1's answers)
DECLARE SUB again ()
DECLARE SUB tick ()
DIM SHARED shared.n
DIM fixed(3)
ON ERROR GOTO caught
OPEN "RERUN.TMP" FOR APPEND AS #1: PRINT #1, "x": CLOSE #1
OPEN "RERUN.TMP" FOR INPUT AS #1
pass = 0
DO WHILE NOT EOF(1): LINE INPUT #1, l$: pass = pass + 1: LOOP
CLOSE #1
READ v
PRINT "pass"; pass; "v"; v; "a"; a; "s$["; s$; "]"; fixed(1); shared.n
tick: tick
a = 7: s$ = "set": fixed(1) = 9: shared.n = 4
OPEN "RERUN2.TMP" FOR OUTPUT AS #2
IF pass = 1 THEN GOSUB restart
IF pass = 2 THEN again
CLOSE #2: KILL "RERUN2.TMP": KILL "RERUN.TMP"
PRINT "end"; a
END
restart:
RUN
caught:
PRINT "error"; ERR
RESUME NEXT
DATA 5, 6

SUB again
  RUN
END SUB

SUB tick STATIC
  n = n + 1
  PRINT "tick"; n
END SUB
