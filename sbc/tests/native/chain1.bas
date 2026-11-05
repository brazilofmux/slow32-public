' CHAIN and COMMON: what goes over, by position, and what stays
' (each program writes the next, so this is one file)
COMMON a%, s$, d#, l&, sg!, arr%(), big#()
DIM arr%(3)
REDIM big#(1 TO 2)
a% = 7: s$ = "hi": d# = 1.5: l& = 100000: sg! = 2.5: arr%(1) = 11: big#(2) = 8.25: lost = 9
OPEN "CHT1.BAS" FOR OUTPUT AS #9
PRINT #9, "COMMON n%, t$, x#, y&, z!, vec%(), wide#()"
PRINT #9, "PRINT n%; t$; x#; y&; z!; vec%(1); UBOUND(vec%); wide#(2); UBOUND(wide#)"
PRINT #9, "PRINT lost"
PRINT #9, "DEF SEG = &H2000: PRINT PEEK(5): DEF SEG"
PRINT #9, "PRINT #1, 55"
PRINT #9, "CLOSE #1: OPEN " + CHR$(34) + "CHT3.TXT" + CHR$(34) + " FOR INPUT AS #1"
PRINT #9, "INPUT #1, v: PRINT v: CLOSE #1"
PRINT #9, "n% = n% + 1"
PRINT #9, "READ w: PRINT w"
PRINT #9, "DATA 6"
PRINT #9, "CHAIN " + CHR$(34) + "CHT2" + CHR$(34)
CLOSE #9
OPEN "CHT2.BAS" FOR OUTPUT AS #9
PRINT #9, "COMMON n%, t$"
PRINT #9, "COMMON more#"
PRINT #9, "PRINT n%; t$; more#; x#"
PRINT #9, "KILL " + CHR$(34) + "CHT1.BAS" + CHR$(34) + ": KILL " + CHR$(34) + "CHT2.BAS" + CHR$(34) + ": KILL " + CHR$(34) + "CHT3.TXT" + CHR$(34)
PRINT #9, "PRINT " + CHR$(34) + "done" + CHR$(34)
PRINT #9, "SYSTEM"
CLOSE #9
DEF SEG = &H2000: POKE 5, 77: DEF SEG
OPEN "CHT3.TXT" FOR OUTPUT AS #1
' a file not there is the handler's, and this program goes on
ON ERROR GOTO none
CHAIN "CHTNOPE.BAS"
PRINT "still here"; e
CHAIN ""
PRINT "still here"; e
ON ERROR GOTO 0
ON TIMER(1) GOSUB tk
TIMER ON
outer
PRINT "not reached"
SYSTEM
none: e = ERR: RESUME NEXT
tk: RETURN

SUB outer
    CHAIN "CHT1.BAS"
END SUB
