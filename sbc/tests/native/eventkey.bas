' ON KEY: a key a trap is on is the trap's, and INKEY$ never sees it; the rest
' stay for INKEY$; STOP keeps one, OFF lets them by.  (QBasic's documentation:
' the oracle's keys reach only the BIOS's buffer, under its traps.)
KEY 15, CHR$(0) + CHR$(30)
KEY 16, CHR$(0) + CHR$(48)
KEY 17, CHR$(3) + CHR$(46)
ON KEY(15) GOSUB ka
ON KEY(16) GOSUB kb
ON KEY(17) GOSUB kc
ON KEY(1) GOSUB f1
ON KEY(11) GOSUB up
KEY(0) STOP
x = 1
KEY(15) ON
KEY(1) ON
KEY(11) ON
KEY(17) ON
waits
PRINT "a"; na; "b"; nb; "C"; nc; "F1"; nf; "up"; nu
PRINT "left ["; INKEY$; INKEY$; INKEY$; "]"
KEY(16) ON
x = 1
PRINT "kept b"; nb
KEY(0) OFF
waits
PRINT "off ["; INKEY$; INKEY$; "]"; na
KEY 1, "hello"
KEY 30, "eleven"
KEY LIST
PRINT "done"
SYSTEM
ka: na = na + 1: RETURN
kb: nb = nb + 1: RETURN
kc: nc = nc + 1: RETURN
f1: nf = nf + 1: RETURN
up: nu = nu + 1: RETURN

SUB waits
    FOR i% = 1 TO 200
        j% = i%
    NEXT
END SUB
