\ tube.fth — vec words for the tube (docs/TUBE.md #3).
\
\ The fence post from docs/plans/1987-desk.md: Forth gets tube words,
\ because Forth is what the arcade was secretly written in. The DVG
\ names live in their own wordlist so MOVE only means "beam" when the
\ tube vocabulary is in the search order:
\
\   cat prelude.fth tube.fth - | ../tools/emulator/slow32-fast kernel.s32x
\   TUBE-ON .          \ -1 = glass ready ( attach s32-crt / s32-crt-mac )
\   ALSO TUBE
\   WIPE 2048 2048 MOVE 3000 3000 DRAW PRESENT
\
\ Coordinates 0..4095, origin lower-left, y up. PRESENT appends END,
\ ships the list, bumps the generation, and wipes for the next frame.

DECIMAL

\ --- display list ---------------------------------------------------
16384 CONSTANT DL-MAX          \ list cap in words (the spec's 65536 is
                               \ the wire cap; 16K is plenty of glow)
CREATE DLIST DL-MAX CELLS ALLOT
VARIABLE DL-N                  \ words built so far
VARIABLE DL-GEN                \ generation counter for the viewer
1 DL-GEN !

: DL, ( u -- ) \ append one raw display-list word
    DL-N @ DL-MAX < IF
        DLIST DL-N @ CELLS + !  1 DL-N +!
    ELSE DROP THEN ;

: XY ( x y op -- u ) \ pack op:31:28 x:27:16 y:15:4
    28 LSHIFT
    SWAP 4095 AND  4 LSHIFT OR
    SWAP 4095 AND 16 LSHIFT OR ;

\ --- service --------------------------------------------------------
: TUBE-ON ( -- flag ) \ negotiate + open vec; degrade to a message
    TUBE-INIT 0< IF ." No tube: attach a screen" CR FALSE EXIT THEN
    1 TUBE-OPEN 0< IF ." tube: vec open failed" CR FALSE EXIT THEN
    TRUE ;

: TUBE-OFF ( -- ) TUBE-CLOSE DROP ;

\ --- viewer keys ----------------------------------------------------
CREATE KEYBUF 4 ALLOT

: GLASS-KEY ( -- code | 0 ) \ next key-down from the glass, 0 if none
    BEGIN
        KEYBUF 4 TUBE-KEYS 0= IF 0 EXIT THEN
        KEYBUF 2 + C@ IF
            KEYBUF C@  KEYBUF 1 + C@ 8 LSHIFT OR  EXIT
        THEN
    AGAIN ;

\ --- the vocabulary -------------------------------------------------
WORDLIST CONSTANT TUBE-WORDLIST

: TUBE ( -- ) \ vocabulary-style: replace top of search order
    GET-ORDER NIP TUBE-WORDLIST SWAP SET-ORDER ;

\ Compile into the tube wordlist, and search it too while we are in
\ here (so PRESENT can see WIPE).
ALSO TUBE  GET-CURRENT TUBE-WORDLIST SET-CURRENT

: MOVE    ( x y -- ) 1 XY DL, ;            \ beam up, go
: DRAW    ( x y -- ) 2 XY DL, ;            \ line to
: POINT   ( x y -- ) 3 XY DL, ;            \ dot, beam stays
: INTEN   ( u -- )   255 AND      [ 4 28 LSHIFT ] LITERAL OR DL, ;
: COLOR   ( rgb -- ) 16777215 AND [ 5 28 LSHIFT ] LITERAL OR DL, ;
: WIPE    ( -- ) 0 DL-N ! ;
: PRESENT ( -- ) \ END, ship, next generation, fresh list
    0 DL,
    DLIST DL-N @ DL-GEN @ TUBE-PRESENT DROP
    1 DL-GEN +!  WIPE ;

SET-CURRENT PREVIOUS
