\ ship.fth — a ship on the scope, flown from the glass, made of words
\ small enough to redefine while it flies.
\
\   cat prelude.fth tube.fth ship.fth - | ../tools/emulator/slow32-fast kernel.s32x
\   ( attach s32-crt or s32-crt-mac in another terminal )
\   FLY
\
\ On the glass: left/right arrows rotate, up thrusts, ESC drops you
\ back at ok> with the ship still in space. Redefine a word and FLY
\ again — position, velocity, and heading persist:
\
\   :NONAME 3 0 DO HDG @ 21 I * + 63 AND SHAPE LOOP ; IS SHIP
\   FLY                            \ now it's three ships in a pinwheel
\
\ This is the trick nothing else on the desk can do: the arcade is
\ still on while you rewire it.

DECIMAL
ALSO TUBE

\ --- fixed-point trig: 64 headings, sin scaled by 256 ---------------
CREATE SINTAB
0 , 25 , 50 , 74 , 98 , 121 , 142 , 162 , 181 , 198 , 213 , 226 , 237 , 245 , 251 , 255 ,
256 , 255 , 251 , 245 , 237 , 226 , 213 , 198 , 181 , 162 , 142 , 121 , 98 , 74 , 50 , 25 ,
0 , -25 , -50 , -74 , -98 , -121 , -142 , -162 , -181 , -198 , -213 , -226 , -237 , -245 , -251 , -255 ,
-256 , -255 , -251 , -245 , -237 , -226 , -213 , -198 , -181 , -162 , -142 , -121 , -98 , -74 , -50 , -25 ,

: SIN64 ( hdg -- s*256 ) 63 AND CELLS SINTAB + @ ;
: COS64 ( hdg -- c*256 ) 16 + SIN64 ;

\ --- ship state: position in 1/16 pixel, so thrust can be gentle ----
VARIABLE X   2048 16 * X !
VARIABLE Y   2048 16 * Y !
VARIABLE VX  0 VX !
VARIABLE VY  0 VY !
VARIABLE HDG 16 HDG !          \ 0=east, 16=north (y is up on a scope)
VARIABLE FLYING

: WRAP ( u -- u ) 65535 AND ;  \ 4096 pixels * 16 subpixels, a torus

: >SCREEN ( -- x y ) X @ 16 / Y @ 16 / ;

: ROTATED ( dx dy hdg -- rx ry ) \ rotate a relative point
    >R 2DUP
    R@ SIN64 * SWAP R@ COS64 * SWAP - 256 /      ( dx dy rx )
    -ROT R@ COS64 * SWAP R> SIN64 * + 256 / ;    ( rx ry )

: AT+ ( rx ry -- x y ) \ translate to the ship, wrap to the scope
    >SCREEN >R ROT + 4095 AND SWAP R> + 4095 AND ;

\ --- the words you are invited to redefine --------------------------
: SHAPE ( hdg -- ) \ one ship outline at this heading
    >R
     60   0 R@ ROTATED AT+ MOVE
    -30  20 R@ ROTATED AT+ DRAW
    -30 -20 R@ ROTATED AT+ DRAW
     60   0 R> ROTATED AT+ DRAW ;

DEFER SHIP
:NONAME ( -- ) HDG @ SHAPE ; IS SHIP

DEFER STEP
:NONAME ( -- )
    X @ VX @ + WRAP X !
    Y @ VY @ + WRAP Y ! ; IS STEP

: THRUST ( -- )
    VX @ HDG @ COS64 8 / + VX !
    VY @ HDG @ SIN64 8 / + VY ! ;

\ --- controls: glass make/break codes (docs/TUBE.md #2) -------------
: KEYS ( -- )
    BEGIN GLASS-KEY ?DUP WHILE
        CASE
            258 OF  2 HDG +!            ENDOF   \ left
            259 OF -2 HDG +!            ENDOF   \ right
            256 OF THRUST               ENDOF   \ up
             27 OF FALSE FLYING !       ENDOF   \ ESC: back to ok>
        ENDCASE
    REPEAT ;

: FRAME ( -- ) WIPE 220 INTEN SHIP PRESENT ;

: FLY ( -- )
    TRUE FLYING !
    BEGIN KEYS STEP FRAME 16 MS FLYING @ 0= UNTIL
    ." ok, the ship holds at " >SCREEN SWAP . . CR ;
