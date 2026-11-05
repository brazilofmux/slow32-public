' PLAY and SOUND, with nothing to hear: what a string may say, how long it
' takes, what PLAY(n) counts; to the second, as a clock tick apart is not the
' point
DECLARE FUNCTION took! (s!)
DECLARE SUB quiet ()
ON ERROR GOTO h
FOR i = 1 TO 44
    READ a$
    e = 0
    PLAY a$
    PRINT "["; a$; "]"; e;
NEXT
PRINT
' (each ends with the queue quiet; each target a whole second, well away
' from where the clock's ticks could round it elsewhere)
s! = TIMER: PLAY "T120 L4 MF CDE": PRINT took!(s!): quiet
s! = TIMER: PLAY "T240 L8 CDEFGABC CDEFGABC C": PRINT took!(s!): quiet
s! = TIMER: PLAY "T120 L4 P4 P4 P4 P4 P4": PRINT took!(s!): quiet
s! = TIMER: PLAY "T120 L4 C. D. C.": PRINT INT(TIMER - s! + .3): quiet
s! = TIMER: PLAY "T120 C2 C2 C8": PRINT took!(s!): quiet
s! = TIMER: PLAY "T120 L4 ML C MS C MN C C C": PRINT took!(s!): quiet
' what stays from one PLAY to the next
s! = TIMER: PLAY "T60 L2": PLAY "CD": PRINT took!(s!): quiet: quiet: quiet
PLAY "T120 L4"
' the background: nothing waited for, the count falling as notes begin
s! = TIMER
PLAY "MB T120 L4 CDEFGAB"
PRINT took!(s!); PLAY(0)
DO WHILE TIMER - s! < 1.25: LOOP
PRINT PLAY(0); PLAY(7)
DO WHILE TIMER - s! < 5: LOOP
PRINT PLAY(0)
' a foreground one waits for the queue
s! = TIMER: PLAY "MB T120 L4 CDEF": PLAY "MF D": PRINT took!(s!); PLAY(0): quiet
' ON PLAY: once, as the count falls under n
s! = TIMER
ON PLAY(2) GOSUB more
PLAY ON
PLAY "MB T120 L4 CDEFG"
DO WHILE TIMER - s! < 3.5: LOOP
PLAY OFF
PLAY "MF"
PRINT n; PLAY(0)
' VARPTR$: X and =
t$ = "T120 L4 CD"
s! = TIMER: PLAY "X" + VARPTR$(t$) + "EF": PRINT took!(s!): quiet
tp% = 120: ln% = 4: sg! = 120.4
s! = TIMER: PLAY "T=" + VARPTR$(tp%) + "L=" + VARPTR$(ln%) + "CDE": PRINT took!(s!): quiet
PLAY "T=" + VARPTR$(sg!): PRINT e: e = 0
lg& = 120: db# = 120
PLAY "T=" + VARPTR$(lg&): PRINT e: e = 0
PLAY "T=" + VARPTR$(db#): PRINT e: e = 0
PLAY "T=" + VARPTR$(t$): PRINT e: e = 0
PLAY "X" + VARPTR$(tp%): PRINT e: e = 0
PLAY "X" + CHR$(3): PRINT e: e = 0
PLAY "C=" + VARPTR$(ln%): PRINT e: e = 0
DIM fx AS STRING * 2, ar$(2)
fx = "CD": ar$(1) = "CD": q$ = "X" + VARPTR$(t$) + "D"
PLAY "X" + VARPTR$(fx): PLAY "X" + VARPTR$(ar$(1)): PLAY "X" + VARPTR$(q$): PRINT e: e = 0
PRINT LEN(VARPTR$(tp%)); ASC(VARPTR$(tp%)); ASC(VARPTR$(lg&)); ASC(VARPTR$(sg!)); ASC(VARPTR$(db#)); ASC(VARPTR$(t$)); ASC(VARPTR$(fx)); ASC(VARPTR$(ar$(0)))
' SOUND does not wait, and has its ranges
quiet
PLAY "T120 L1 CD": s! = TIMER: SOUND 440, 1: PRINT took!(s!): quiet
SOUND 37, 1: SOUND 32767, 1: SOUND 440, 0: SOUND 440, 65535: PRINT e
SOUND 36, 1: PRINT e: e = 0
SOUND 32768, 1: PRINT e: e = 0
SOUND 440, -1: PRINT e: e = 0
SOUND 440, 65536: PRINT e: e = 0
PRINT "done"
SYSTEM
more: n = n + 1: RETURN
h: e = ERR: RESUME NEXT
DATA H,L0,L65,T31,T256,T32,T255,O7,O6,O0,N85,N-1,P0,P65,C65,C0,M,MX,>>>>>>>>,<<<<<<<<,X,"","   "
DATA C+-,1,c d e,L4;C;D,=,Oa,B-,E+,F-,"T255 L64 C+",C-,D+,D-,E-,F+,G+,G-,A+,A-,B+,E#
FUNCTION took! (s!)
    took! = INT(TIMER - s! + .5)
END FUNCTION
SUB quiet
    q! = TIMER
    DO WHILE TIMER - q! < .7: LOOP
END SUB
