' GOSUB in a SUB and a FUNCTION, as QBasic 1.1 runs it: the routine is
' the procedure's own, ON GOSUB too (RETURN label QBasic will not load
' in one: Illegal in procedure or DEF FN), a recursion's GOSUBs
' each its own, EXIT SUB from inside a routine (its GOSUB dropped), and
' RETURN without one of its own while the caller has one waiting
DECLARE SUB walk (n%)
DECLARE SUB pick (k%)
DECLARE SUB bail ()
DECLARE SUB stray ()
DECLARE FUNCTION twice% (n%)
ON ERROR GOTO caught
GOSUB outer
PRINT "main again"
walk 3
pick 2: pick 1
PRINT twice(5)
bail
PRINT "after bail"
GOSUB withstray
PRINT "done"; e
END
outer:
PRINT "outer";
walk 1
RETURN
withstray:
stray
RETURN
caught:
e = ERR
RESUME NEXT

SUB walk (n%)
  IF n% = 0 THEN EXIT SUB
  GOSUB step1
  walk n% - 1
  PRINT "back"; n%
  EXIT SUB
step1:
  PRINT " step"; n%;
  RETURN
END SUB

SUB pick (k%)
  ON k% GOSUB one, two
  PRINT " picked"; k%
  EXIT SUB
one: PRINT "one";: RETURN
two: PRINT "two";: RETURN
END SUB

FUNCTION twice% (n%)
  GOSUB dbl
  EXIT FUNCTION
dbl:
  twice = n% * 2
  RETURN
END FUNCTION

SUB bail
  GOSUB away
  PRINT "not here"
  EXIT SUB
away:
  PRINT "bailing"
  EXIT SUB
END SUB

SUB stray
  RETURN
END SUB
