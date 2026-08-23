' BOUNCE.BAS - the GW-BASIC graphics statements, on the tube.
'
' Run it, then attach a glass:
'   cd sbasic && ../tools/emulator/slow32-fast sbasic.s32x demos/bounce.bas
'   (in another terminal) ../tools/s32-crt-mac   or   s32-crt
'
' Q or ESC on the glass (or the console) quits.

SCREEN 1

' the walls
LINE (0, 0)-(319, 199), 8, B

x = 160: y = 100
dx = 2.6: dy = 1.7
r = 7
c = 2

DO
    ' draw the ball
    CIRCLE (x, y), r, c
    PAINT STEP(0, 0), c, c

    SLEEP 0.02

    ' erase it (the walls live outside the bounce margin)
    LINE (x - r, y - r)-(x + r, y + r), 0, BF

    x = x + dx
    y = y + dy
    bounced = 0
    IF x < r + 2 OR x > 317 - r THEN dx = -dx: bounced = 1
    IF y < r + 2 OR y > 197 - r THEN dy = -dy: bounced = 1
    IF x < r + 2 THEN x = r + 2
    IF x > 317 - r THEN x = 317 - r
    IF y < r + 2 THEN y = r + 2
    IF y > 197 - r THEN y = 197 - r
    IF bounced THEN c = c + 1: IF c > 15 THEN c = 1

    k$ = INKEY$
LOOP UNTIL k$ = "q" OR k$ = "Q" OR k$ = CHR$(27)

SCREEN 0
PRINT "bounce: done"
