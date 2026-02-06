: \ NTIB @ TOIN ! ; IMMEDIATE
\ First definition MUST be backslash-comment.
\ Everything after this can use \ for comments.

\ --- Stack manipulation ---
: ROT    >R SWAP R> SWAP ;
: -ROT   SWAP >R SWAP R> ;
: NIP    SWAP DROP ;
: TUCK   SWAP OVER ;
: 2DUP   OVER OVER ;
: 2DROP  DROP DROP ;
: 2SWAP  >R -ROT R> -ROT ;
: ?DUP   DUP IF DUP THEN ;

\ --- Arithmetic ---
: ABS    DUP 0< IF NEGATE THEN ;
: MIN    2DUP > IF SWAP THEN DROP ;
: MAX    2DUP < IF SWAP THEN DROP ;

\ --- Constants ---
: TRUE   1 ;
: FALSE  0 ;
: BL     32 ;

\ --- Cell operations (32-bit, cell = 4 bytes) ---
: CELLS  4 * ;
: CELL+  4 + ;
: CHARS  ;
: CHAR+  1+ ;

\ --- Output ---
: SPACE   BL EMIT ;
: SPACES  BEGIN DUP 0 > WHILE SPACE 1- REPEAT DROP ;

\ --- Number base ---
: DECIMAL  10 BASE ! ;
: HEX      16 BASE ! ;

\ --- Memory ---
: +!  DUP @ ROT + SWAP ! ;

\ --- Comparison extensions ---
: <=  > 0= ;
: >=  < 0= ;

\ Enable interactive prompts
PROMPTS-ON
