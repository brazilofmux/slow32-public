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
: 2OVER  >R >R 2DUP R> R> 2SWAP ;
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
: 2VARIABLE  CREATE 0 , 0 , ;
: 2CONSTANT  CREATE , , DOES> 2@ ;

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

\ --- Pictured Numeric Output (double-cell) ---
: MU/MOD  ( ud u -- rem udquot )
  >R 0 R@ UM/MOD -ROT R> UM/MOD ROT ;

: #  ( ud -- ud' )
  BASE @ MU/MOD ROT
  DUP 10 < IF 48 + ELSE 10 - 65 + THEN
  HOLD ;

: #S  ( ud -- 0 0 )
  BEGIN # 2DUP OR 0= UNTIL ;

: SIGN  ( n -- )
  0< IF 45 HOLD THEN ;

: U.  ( u -- )
  0 <# #S #> TYPE SPACE ;

: .R  ( n width -- )
  SWAP DUP >R ABS 0
  <# #S R> SIGN #>
  ROT OVER - SPACES TYPE ;

: U.R  ( u width -- )
  >R 0 <# #S #> R> OVER - SPACES TYPE ;

\ --- String Utilities ---
: CMOVE  ( src dest u -- )
  DUP 0 > IF 0 DO  OVER I + C@  OVER I + C!  LOOP ELSE DROP THEN 2DROP ;

: CMOVE>  ( src dest u -- )
  BEGIN DUP 0 > WHILE
    1- >R  OVER R@ + C@  OVER R@ + C!  R>
  REPEAT DROP 2DROP ;

: /STRING  ( addr u n -- addr+n u-n )
  ROT OVER + ROT ROT - ;

: COMPARE  ( a1 u1 a2 u2 -- n )
  ROT 2DUP SWAP - >R MIN    ( a1 a2 min-len  R: u1-u2 )
  DUP 0 > IF
    0 DO
      OVER I + C@  OVER I + C@  -
      ?DUP IF
        0< IF 2DROP R> DROP -1 ELSE 2DROP R> DROP 1 THEN
        UNLOOP EXIT
      THEN
    LOOP
  ELSE DROP THEN
  2DROP
  R> DUP IF 0< IF DROP -1 ELSE DROP 1 THEN THEN ;

: (STREQ)  ( a1 a2 u -- flag )
  DUP 0 > IF
    0 DO
      OVER I + C@  OVER I + C@ <> IF 2DROP 0 UNLOOP EXIT THEN
    LOOP
  ELSE DROP THEN
  2DROP 1 ;

: SEARCH  ( a1 u1 a2 u2 -- a3 u3 flag )
  DUP 0= IF 2DROP 1 EXIT THEN
  ROT >R ROT R>                ( a2 u2 a1 u1 )
  DUP 3 PICK - 1+ 0 MAX       ( a2 u2 a1 u1 limit )
  DUP 0 > IF
    0 DO
      OVER I +                   ( a2 u2 a1 u1 a1+i )
      4 PICK 4 PICK              ( a2 u2 a1 u1 a1+i a2 u2 )
      (STREQ) IF
        SWAP I + SWAP I -        ( a2 u2 a1+i u1-i )
        2SWAP 2DROP 1
        UNLOOP EXIT
      THEN
    LOOP
  ELSE DROP THEN
  2SWAP 2DROP 0 ;

: PLACE  ( addr u dest -- )
  2DUP C!  1+ SWAP CMOVE ;

: -TRAILING  ( addr u -- addr u' )
  BEGIN DUP 0 > WHILE
    2DUP + 1- C@ BL <> IF EXIT THEN 1-
  REPEAT ;

\ --- Double-Number Arithmetic ---
: D>S  ( d -- n )  DROP ;
: DNEGATE  ( d -- -d )  INVERT SWAP INVERT SWAP 1 0 D+ ;
: DABS  ( d -- |d| )  DUP 0< IF DNEGATE THEN ;
: D0=  ( d -- flag )  OR 0= ;
: D0<  ( d -- flag )  NIP 0< ;
: D=  ( d1 d2 -- flag )  D- D0= ;
: D<  ( d1 d2 -- flag )  ROT 2DUP = IF 2DROP U< ELSE > NIP NIP THEN ;
: M+  ( d n -- d )  S>D D+ ;
: D.  ( d -- )  DUP >R DABS <# #S R> SIGN #> TYPE SPACE ;
: D.R  ( d width -- )  >R DUP >R DABS <# #S R> SIGN #> R> OVER - SPACES TYPE ;

\ Enable interactive prompts
PROMPTS-ON
