\ hello.fth — the forthc M1 gate program.
\ Pure-stack words, literals large and small, nested calls, EMIT/./CR.

: STAR 42 EMIT ;
: STARS STAR STAR STAR ;

: BANNER
  72 EMIT 101 EMIT 108 EMIT 108 EMIT 111 EMIT 44 EMIT 32 EMIT
  99 104 116 114 111 102
  EMIT EMIT EMIT EMIT EMIT EMIT
  33 EMIT CR ;

: MATH
  6 7 * . CR
  10 3 - . CR
  -42 . CR
  0 . CR
  100000 . CR          ( needs lui+addi )
  -100000 . CR
  1 2 SWAP - . CR      ( 2-1 = 1 )
  5 DUP + . CR
  3 4 OVER + + . CR    ( 3 4 3 -> 10 )
  9 8 DROP . CR ;

: MAIN BANNER STARS CR MATH STARS CR ;
