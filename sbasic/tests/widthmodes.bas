' WIDTH in the graphics modes as QBasic takes it on a VGA: 80 makes
' SCREEN 1 into 2, 7 into 8 and 13 into text; 40 makes 2 into 1 and 8
' or 9 into 7; a change clears, the same width does not; 40 in 12 is
' refused.  Then the colors: a color's low byte, the mode's last if
' past them; PALETTE's attributes and values per mode (16 colors at 200
' lines, 64 in 9, VGA's b*65536+g*256+r at 480 and in 13) -- the
' values printed last
DIM r(300), e$(1)
ON ERROR GOTO caught
DEF SEG = 0
k = 0: SCREEN 1: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 1: SCREEN 7: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 2: SCREEN 13: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 3: SCREEN 2: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 4: SCREEN 8: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 5: SCREEN 9: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 6: SCREEN 12: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 7: SCREEN 11: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 8: SCREEN 1: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 9: SCREEN 7: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 10: SCREEN 13: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 11: SCREEN 2: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 12: SCREEN 9: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 80
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 13: SCREEN 12: PSET (5, 5), 3: LOCATE 2, 1: PRINT "Hi";
WIDTH 40
r(n) = PEEK(&H449): r(n + 1) = PEEK(&H44A): r(n + 2) = POINT(5, 5): r(n + 3) = SCREEN(2, 1): r(n + 4) = CSRLIN: r(n + 5) = POS(0)
r(n + 6) = POINT(600, 5): r(n + 7) = POINT(5, 300)
PSET (600, 5), 1: r(n + 8) = POINT(600, 5): PSET (7, 7), 15: r(n + 9) = POINT(7, 7): n = n + 10
k = 30: SCREEN 1: PSET (3, 3), 2: r(n) = POINT(3, 3): n = n + 1
k = 31: SCREEN 1: PSET (3, 3), 4: r(n) = POINT(3, 3): n = n + 1
k = 32: SCREEN 1: PSET (3, 3), 5: r(n) = POINT(3, 3): n = n + 1
k = 33: SCREEN 1: PSET (3, 3), 255: r(n) = POINT(3, 3): n = n + 1
k = 34: SCREEN 1: PSET (3, 3), 256: r(n) = POINT(3, 3): n = n + 1
k = 35: SCREEN 1: PSET (3, 3), 32767: r(n) = POINT(3, 3): n = n + 1
k = 36: SCREEN 1: PSET (3, 3), -1: r(n) = POINT(3, 3): n = n + 1
k = 37: SCREEN 2: PSET (3, 3), 2: r(n) = POINT(3, 3): n = n + 1
k = 38: SCREEN 2: PSET (3, 3), 1: r(n) = POINT(3, 3): n = n + 1
k = 39: SCREEN 7: PSET (3, 3), 16: r(n) = POINT(3, 3): n = n + 1
k = 40: SCREEN 7: PSET (3, 3), 17: r(n) = POINT(3, 3): n = n + 1
k = 41: SCREEN 7: PSET (3, 3), 31: r(n) = POINT(3, 3): n = n + 1
k = 42: SCREEN 7: PSET (3, 3), 255: r(n) = POINT(3, 3): n = n + 1
k = 43: SCREEN 13: PSET (3, 3), 255: r(n) = POINT(3, 3): n = n + 1
k = 44: SCREEN 13: PSET (3, 3), 256: r(n) = POINT(3, 3): n = n + 1
k = 45: SCREEN 13: PSET (3, 3), 300: r(n) = POINT(3, 3): n = n + 1
k = 46: SCREEN 12: PSET (3, 3), 16: r(n) = POINT(3, 3): n = n + 1
k = 47: SCREEN 11: PSET (3, 3), 2: r(n) = POINT(3, 3): n = n + 1
k = 48: SCREEN 9: PSET (3, 3), 16: r(n) = POINT(3, 3): n = n + 1
k = 60: SCREEN 1: PALETTE 3, 15
k = 61: SCREEN 1: PALETTE 3, 16
k = 62: SCREEN 1: PALETTE 4, 0
k = 63: SCREEN 2: PALETTE 1, 15
k = 64: SCREEN 2: PALETTE 1, 16
k = 65: SCREEN 2: PALETTE 2, 0
k = 66: SCREEN 7: PALETTE 15, 15
k = 67: SCREEN 7: PALETTE 15, 16
k = 68: SCREEN 8: PALETTE 1, 16
k = 69: SCREEN 9: PALETTE 1, 63
k = 70: SCREEN 9: PALETTE 1, 64
k = 71: SCREEN 11: PALETTE 1, 4144959
k = 72: SCREEN 12: PALETTE 15, 64
k = 73: SCREEN 13: PALETTE 0, 4144960
k = 74: SCREEN 13: PALETTE 256, 0
k = 75: SCREEN 13: PALETTE 0, -2
k = 76: SCREEN 12: PALETTE 0, -1
k = 77: SCREEN 0: PALETTE 3, 16
k = 78: SCREEN 0: PALETTE 16, 1
SCREEN 0: WIDTH 80
FOR i = 0 TO n - 1: PRINT r(i);: IF i MOD 10 = 9 THEN PRINT
NEXT
PRINT
PRINT e$(0)
END
caught:
e$(0) = e$(0) + STR$(k) + ":" + MID$(STR$(ERR), 2)
RESUME NEXT
