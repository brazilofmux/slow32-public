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

\ --- Defining words ---
: VARIABLE  CREATE 0 , ;
: CONSTANT  CREATE , DOES> @ ;

\ --- CASE/OF/ENDOF/ENDCASE ---
\ Usage: ( n ) CASE  1 OF ." one" ENDOF  2 OF ." two" ENDOF  ." default" ENDCASE
: CASE    0 ; IMMEDIATE
: OF      POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP ; IMMEDIATE
: ENDOF   POSTPONE ELSE ; IMMEDIATE
: ENDCASE POSTPONE DROP  BEGIN ?DUP WHILE POSTPONE THEN REPEAT ; IMMEDIATE

\ --- VALUE / TO ---
\ Usage: 42 VALUE X   X .   99 TO X   X .
\ Body is at XT+8 for all CREATE/DOES> words (skip codeword + does-cell).
: VALUE   CREATE , DOES> @ ;
: TO  ' 8 +  STATE @ IF POSTPONE LIT , POSTPONE !  ELSE !  THEN ; IMMEDIATE

\ --- Deferred words ---
\ Usage: DEFER GREET   : HI ." hi" CR ;   ' HI IS GREET   GREET
: DEFER      CREATE 0 , DOES> @ EXECUTE ;
: IS         ' 8 +  STATE @ IF POSTPONE LIT , POSTPONE !  ELSE !  THEN ; IMMEDIATE
: ACTION-OF  ' 8 +  STATE @ IF POSTPONE LIT , POSTPONE @  ELSE @  THEN ; IMMEDIATE

\ --- Paren comments ---
: (  BEGIN
       TOIN @ NTIB @ < WHILE
       TIB TOIN @ + C@   1 TOIN +!
       41 = IF EXIT THEN
     REPEAT ; IMMEDIATE

\ --- Compile-time literal ---
: LITERAL  ['] LIT , , ; IMMEDIATE

\ --- Comparison extensions ---
: 0>   0 > ;
: 0<>  0 <> ;

\ --- Memory operations ---
: FILL  ( addr u char -- )
  -ROT  0 DO 2DUP I + C! LOOP 2DROP ;

: ERASE  ( addr u -- )  0 FILL ;

: MOVE  ( src dest u -- )
  DUP 0= IF DROP 2DROP EXIT THEN
  >R 2DUP U< IF
    \ src < dest: copy backward (high to low) for overlap safety
    R> 1-  BEGIN  DUP 0< 0= WHILE
      >R  OVER R@ + C@  OVER R@ + C!  R> 1-
    REPEAT DROP
  ELSE
    \ dest <= src: copy forward (low to high)
    R> 0 DO  OVER I + C@  OVER I + C!  LOOP
  THEN 2DROP ;

\ --- Pictured Numeric Output ---
: #  ( u -- u' )
  BASE @ /MOD SWAP
  DUP 10 < IF 48 + ELSE 10 - 65 + THEN
  HOLD ;

: #S  ( u -- 0 )
  BEGIN # DUP 0= UNTIL ;

: SIGN  ( n -- )
  0< IF 45 HOLD THEN ;

: U.  ( u -- )
  <# #S #> TYPE SPACE ;

: .R  ( n width -- )
  SWAP DUP >R ABS
  <# #S R> SIGN #>
  ROT OVER - SPACES TYPE ;

: U.R  ( u width -- )
  >R <# #S #> R> OVER - SPACES TYPE ;

\ Enable interactive prompts
PROMPTS-ON
