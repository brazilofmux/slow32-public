\ prelude-fc.fth — the closed-world prelude: kernel-vocabulary words
\ defined in compilable Forth. Concatenated ahead of user source by
\ compile.sh (the way prelude.fth precedes kernel input). Where the
\ kernel's flag conventions are quirky (TRUE=-1 but comparisons give
\ 0/1), these mirror the kernel exactly — the DTC is M4's oracle.

: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: ROT >R SWAP R> SWAP ;
: -ROT ROT ROT ;
: 2DUP OVER OVER ;
: 2DROP DROP DROP ;
: 2SWAP >R -ROT R> -ROT ;
: 2OVER >R >R 2DUP R> R> 2SWAP ;
: ?DUP DUP IF DUP THEN ;

: NEGATE 0 SWAP - ;
: INVERT -1 XOR ;
: ABS DUP 0< IF NEGATE THEN ;
: MIN 2DUP > IF SWAP THEN DROP ;
: MAX 2DUP < IF SWAP THEN DROP ;
: 2* DUP + ;
: 0> 0 > ;
: U> SWAP U< ;
: WITHIN OVER - >R - R> U< ;

: TRUE -1 ;
: FALSE 0 ;
32 CONSTANT BL

: CELLS 4 * ;
: CELL+ 4 + ;
: CHARS ;
: CHAR+ 1+ ;
: ALIGNED 3 + -4 AND ;

: +! DUP @ ROT + SWAP ! ;

\ double-cell words: the kernel prelude's own definitions, verbatim
: D>S DROP ;
: DNEGATE INVERT SWAP INVERT SWAP 1 0 D+ ;
: DABS DUP 0< IF DNEGATE THEN ;
: D0= OR 0= ;
: D0< NIP 0< ;
: D= D- D0= ;
: D< ROT 2DUP = IF 2DROP U< ELSE > NIP NIP THEN ;
: M+ S>D D+ ;

\ division family: the kernel prelude's own definitions, verbatim
: MU/MOD >R 0 R@ UM/MOD -ROT R> UM/MOD ROT ;
: SM/REM
  2DUP XOR >R
  OVER >R
  ABS >R DABS R>
  UM/MOD
  R> 0< IF SWAP NEGATE SWAP THEN
  R> 0< IF NEGATE THEN ;
: FM/MOD
  DUP >R
  SM/REM
  DUP 0< IF
    OVER IF
      1- SWAP R> + SWAP EXIT
    THEN
  THEN
  R> DROP ;
: */MOD >R M* R> FM/MOD ;
: */ */MOD NIP ;

: CMOVE 0 ?DO OVER I + C@ OVER I + C! LOOP 2DROP ;
: CMOVE> BEGIN DUP 0<> WHILE
    1- >R OVER R@ + C@ OVER R@ + C! R>
  REPEAT DROP 2DROP ;
: /STRING TUCK - >R + R> ;

\ string words: the kernel prelude's own definitions, verbatim
: COMPARE
  ROT 2DUP SWAP - >R MIN
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
  R> DUP IF 0< IF -1 ELSE 1 THEN THEN ;
: PLACE 2DUP C! 1+ SWAP CMOVE ;
: -TRAILING
  BEGIN DUP 0 > WHILE
    2DUP + 1- C@ BL <> IF EXIT THEN 1-
  REPEAT ;

VARIABLE srch-a2  VARIABLE srch-u2
: SEARCH ( a1 u1 a2 u2 -- a3 u3 flag )   \ kernel flag convention: 0/1
  DUP 0= IF 2DROP 1 EXIT THEN
  srch-u2 ! srch-a2 !
  DUP srch-u2 @ < IF 0 EXIT THEN
  DUP srch-u2 @ - 1+ 0 ?DO
    OVER I +  srch-u2 @  srch-a2 @  srch-u2 @  COMPARE 0= IF
      SWAP I + SWAP I -  1 UNLOOP EXIT THEN
  LOOP 0 ;
: SPACE BL EMIT ;
: SPACES BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP ;
: TYPE 0 ?DO DUP I + C@ EMIT LOOP DROP ;
: COUNT DUP 1+ SWAP C@ ;
: ERASE 0 FILL ;
: BLANK BL FILL ;

\ minimal pictured output, decimal only (closed world has no BASE)
CREATE pno 68 ALLOT  VARIABLE hld
: <# pno 68 + hld ! ;
: HOLD -1 hld +! hld @ C! ;
: #> 2DROP hld @ pno 68 + hld @ - ;
: # 10 MU/MOD ROT 48 + HOLD ;
: #S BEGIN # 2DUP OR 0= UNTIL ;
: SIGN 0< IF 45 HOLD THEN ;
: D. DUP >R DABS <# #S R> SIGN #> TYPE SPACE ;
: U. 0 <# #S #> TYPE SPACE ;
: D.R >R DUP >R DABS <# #S R> SIGN #> R> OVER - SPACES TYPE ;
: .R >R S>D DUP >R DABS <# #S R> SIGN #> R> OVER - SPACES TYPE ;
: U.R >R 0 <# #S #> R> OVER - SPACES TYPE ;
