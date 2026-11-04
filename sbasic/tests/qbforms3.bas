' QuickBASIC's forms, as QBasic 1.1 runs them: names with a dot (a
' variable, a label, a SUB) beside a record's fields, SUBs named as the
' interpreter's commands (SAVE, NEW) and a label NEW, SUB and FUNCTION
' ... STATIC, STATIC x AS INTEGER, a one-line ELSEIF, the label 1.1,
' SCREEN with its mode left out, and RUN (the second run sees a file)
DECLARE SUB summer.day ()
DECLARE SUB save (t)
DECLARE SUB tally ()
DECLARE FUNCTION nextid% ()
TYPE spot
  x AS INTEGER
  y AS INTEGER
END TYPE
DIM SHARED t.count
DIM here AS spot
OPEN "QBF3.TMP" FOR APPEND AS #1: PRINT #1, "x": CLOSE #1
OPEN "QBF3.TMP" FOR INPUT AS #1
runs = 0
DO WHILE NOT EOF(1): LINE INPUT #1, l$: runs = runs + 1: LOOP
CLOSE #1
IF runs = 1 THEN PRINT "first run": RUN
KILL "QBF3.TMP"
PRINT "run"; runs
here.x = 4: here.y = here.x * 2
t.count = 3: half.way = 2.5
PRINT here.x; here.y; t.count; half.way
CALL summer.day
GOSUB load.sprites
k = 0: save 11      ' (mid-line: a line starting SAVE is the REPL's, piped in)
tally: tally: tally
PRINT nextid; nextid%; nextid
k = 2
IF k = 1 THEN
  PRINT "one"
ELSEIF k = 2 THEN PRINT "two": PRINT "still two"
ELSE
  PRINT "other"
END IF
SCREEN , , 0, 0
GOTO 1.1
PRINT "skipped"
1.1
PRINT "at 1.1"
GOTO new
PRINT "skipped too"
new:
PRINT "at new"
END
load.sprites:
PRINT "sprites"; t.count
RETURN

SUB summer.day
  PRINT "summer"; t.count
END SUB

SUB save (t)
  PRINT "save"; t
END SUB

SUB tally STATIC
  n = n + 1
  DIM marks(3)
  marks(n) = n * 10
  PRINT "tally"; n; marks(1)
END SUB

FUNCTION nextid% STATIC
  STATIC last AS INTEGER
  last = last + 1.4
  nextid = last
END FUNCTION
