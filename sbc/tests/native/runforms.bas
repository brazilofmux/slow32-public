' RUN: alone, from a line, another program; what starts again and what stays.
' Memory stays, so that says which run this is
DECLARE SUB fromsub ()
COMMON cm%
DEF SEG = &H9000
IF PEEK(1) <> 171 THEN POKE 0, 0: POKE 1, 171
stage = PEEK(0)
POKE 0, stage + 1
DEF SEG
n% = n% + 1
RANDOMIZE 5
r0 = RND
PRINT "top"; stage; n%
ON ERROR GOTO oops
SELECT CASE stage
CASE 0: OPEN "RUNT1.TXT" FOR OUTPUT AS #1: RUN
CASE 1
    a%(1) = 4
    PRINT #1, 5
    PRINT "closed"; e
    e = 0
    RUN 40
CASE 2: fromsub
CASE 3
    OPEN "RUNT2.TMP" FOR OUTPUT AS #9
    PRINT #9, "COMMON cm%"
    PRINT #9, "PRINT " + CHR$(34) + "ran" + CHR$(34) + "; cm%; zz%"
    PRINT #9, "DEF SEG = &H9000: PRINT PEEK(0): POKE 1, 0: DEF SEG"
    PRINT #9, "ON ERROR GOTO 900"
    PRINT #9, "PRINT #1, 5"
    PRINT #9, "KILL " + CHR$(34) + "RUNT1.TXT" + CHR$(34)
    PRINT #9, "KILL " + CHR$(34) + "RUNT2.TMP" + CHR$(34)
    PRINT #9, "PRINT " + CHR$(34) + "done" + CHR$(34)
    PRINT #9, "SYSTEM"
    PRINT #9, "900 PRINT " + CHR$(34) + "E" + CHR$(34) + "; ERR: RESUME NEXT"
    CLOSE #9
    OPEN "RUNT1.TXT" FOR OUTPUT AS #1
    RUN "RUNTNOPE.BAS"
    PRINT "missing"; e
    cm% = 3: zz% = 4
    a$ = "RUNT2"
    RUN a$ + ".TMP"
END SELECT
PRINT "not here"
SYSTEM
40 PRINT "at 40"; n%; a%(1); e
DIM arr%(2): PRINT LBOUND(arr%)
DEF SEG = &H9100: POKE 0, 1
RUN 50
PRINT "not here"
SYSTEM
50 PRINT PEEK(0) = 112 OR PEEK(0) = 0
DEF SEG = &H9000: POKE 0, 2: DEF SEG
RUN
60 PRINT "from sub"
DEF SEG = &H9000: POKE 0, 3: DEF SEG
RUN
oops: e = ERR: RESUME NEXT

SUB fromsub
    PRINT "in sub"
    RUN 60
END SUB
