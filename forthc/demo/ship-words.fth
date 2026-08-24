\ ship-words.fth — the tube vocabulary and the ship, closed-world.
\ Word bodies are verbatim from forth/tube.fth and forth/ship.fth so
\ the compiled frames hash identical to the DTC kernel's (that is the
\ M5 gate). Compile --hosted: the tube words ride the C runtime.

\ --- display list (forth/tube.fth) -----------------------------------
16384 CONSTANT DL-MAX
CREATE DLIST 65536 ALLOT
VARIABLE DL-N
VARIABLE DL-GEN
1 DL-GEN !

: DL, DL-N @ DL-MAX < IF
        DLIST DL-N @ CELLS + !  1 DL-N +!
    ELSE DROP THEN ;

: XY ( x y op -- u )
    28 LSHIFT
    SWAP 4095 AND  4 LSHIFT OR
    SWAP 4095 AND 16 LSHIFT OR ;

: TUBE-ON ( -- flag )
    TUBE-INIT 0< IF ." No tube: attach a screen" CR FALSE EXIT THEN
    1 TUBE-OPEN 0< IF ." tube: vec open failed" CR FALSE EXIT THEN
    TRUE ;

CREATE KEYBUF 4 ALLOT
: GLASS-KEY ( -- code | 0 )
    BEGIN
        KEYBUF 4 TUBE-KEYS 0= IF 0 EXIT THEN
        KEYBUF 2 + C@ IF
            KEYBUF C@  KEYBUF 1 + C@ 8 LSHIFT OR  EXIT
        THEN
    AGAIN ;

: MOVE    1 XY DL, ;
: DRAW    2 XY DL, ;
: POINT   3 XY DL, ;
: INTEN   255 AND      1073741824 OR DL, ;
: COLOR   16777215 AND 1342177280 OR DL, ;
: WIPE    0 DL-N ! ;
: PRESENT
    0 DL,
    DLIST DL-N @ DL-GEN @ TUBE-PRESENT DROP
    1 DL-GEN +!  WIPE ;

\ --- the ship (forth/ship.fth) ---------------------------------------
CREATE SINTAB
0 , 25 , 50 , 74 , 98 , 121 , 142 , 162 , 181 , 198 , 213 , 226 , 237 , 245 , 251 , 255 ,
256 , 255 , 251 , 245 , 237 , 226 , 213 , 198 , 181 , 162 , 142 , 121 , 98 , 74 , 50 , 25 ,
0 , -25 , -50 , -74 , -98 , -121 , -142 , -162 , -181 , -198 , -213 , -226 , -237 , -245 , -251 , -255 ,
-256 , -255 , -251 , -245 , -237 , -226 , -213 , -198 , -181 , -162 , -142 , -121 , -98 , -74 , -50 , -25 ,

: SIN64 63 AND CELLS SINTAB + @ ;
: COS64 16 + SIN64 ;

VARIABLE X   2048 16 * X !
VARIABLE Y   2048 16 * Y !
VARIABLE VX  0 VX !
VARIABLE VY  0 VY !
VARIABLE HDG 16 HDG !
VARIABLE FLYING

: WRAP 65535 AND ;

: >SCREEN X @ 16 / Y @ 16 / ;

: ROTATED ( dx dy hdg -- rx ry )
    >R 2DUP
    R@ SIN64 * SWAP R@ COS64 * SWAP - 256 /
    -ROT R@ COS64 * SWAP R> SIN64 * + 256 / ;

: AT+ ( rx ry -- x y )
    >SCREEN >R ROT + 4095 AND SWAP R> + 4095 AND ;

: SHAPE ( hdg -- )
    >R
     60   0 R@ ROTATED AT+ MOVE
    -30  20 R@ ROTATED AT+ DRAW
    -30 -20 R@ ROTATED AT+ DRAW
     60   0 R> ROTATED AT+ DRAW ;

DEFER SHIP
: SHIP-ONE HDG @ SHAPE ;
' SHIP-ONE IS SHIP

DEFER STEP
: STEP-FLY
    X @ VX @ + WRAP X !
    Y @ VY @ + WRAP Y ! ;
' STEP-FLY IS STEP

: THRUST
    VX @ HDG @ COS64 8 / + VX !
    VY @ HDG @ SIN64 8 / + VY ! ;

: KEYS
    BEGIN GLASS-KEY ?DUP WHILE
        CASE
            258 OF  2 HDG +!            ENDOF
            259 OF -2 HDG +!            ENDOF
            256 OF THRUST               ENDOF
             27 OF FALSE FLYING !       ENDOF
        ENDCASE
    REPEAT ;

: FRAME WIPE 220 INTEN SHIP PRESENT ;
