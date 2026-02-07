\ Test core utilities, loops & parsing

\ --- ?DO (zero-trip loop) ---
: T-QDO1  0 0 ?DO 99 . LOOP ." empty" CR ;
T-QDO1

: T-QDO2  3 0 ?DO I . LOOP CR ;
T-QDO2

: T-QDO3  5 5 ?DO 88 . LOOP ." skip" CR ;
T-QDO3

: T-QDO4  1 0 ?DO ." once" LOOP CR ;
T-QDO4

\ Nested ?DO with DO
: T-QDO5  3 0 ?DO 2 0 DO I J * . LOOP LOOP CR ;
T-QDO5

\ ?DO with LEAVE
: T-QDO6  10 0 ?DO I DUP 3 = IF DROP LEAVE THEN . LOOP ." left" CR ;
T-QDO6

\ --- PARSE ---
\ PARSE returns text starting from >IN (which includes space after word name)
: T-PARSE1  41 PARSE TYPE CR ;
T-PARSE1 hello)

: T-PARSE2  41 PARSE TYPE ." |" CR ;
T-PARSE2 )

\ --- 2/ (arithmetic shift right) ---
6 2/ . CR
-6 2/ . CR
-7 2/ . CR
0 2/ . CR
1 2/ . CR

\ --- 2* ---
3 2* . CR
-3 2* . CR
0 2* . CR

\ --- U> ---
5 3 U> . CR
3 5 U> . CR
3 3 U> . CR

\ --- MARKER ---
MARKER -TEMP-
VARIABLE TEMPVAR
42 TEMPVAR !
TEMPVAR @ . CR
-TEMP-
\ After marker, TEMPVAR should be gone. Use [DEFINED] to check.
[DEFINED] TEMPVAR . CR

\ --- .( immediate display ---
: T-DOTPAREN .( hello) ;
T-DOTPAREN CR

\ --- BUFFER: ---
16 BUFFER: MYBUF
65 MYBUF C!
MYBUF C@ . CR

\ --- NOOP ---
1 NOOP . CR

\ --- ROLL ---
10 20 30 2 ROLL . . . CR
10 20 0 ROLL . . CR
10 20 30 1 ROLL . . . CR

\ --- HOLDS ---
: T-HOLDS  0 <# S" xyz" HOLDS #> TYPE CR ;
T-HOLDS

\ --- [DEFINED] / [UNDEFINED] ---
[DEFINED] DUP . CR
[UNDEFINED] DUP . CR
[DEFINED] XYZZY-NONEXIST . CR
[UNDEFINED] XYZZY-NONEXIST . CR

\ --- BLANK ---
CREATE BLKBUF 4 ALLOT
65 BLKBUF C!
66 BLKBUF 1+ C!
67 BLKBUF 2 + C!
68 BLKBUF 3 + C!
BLKBUF 4 BLANK
BLKBUF C@ . BLKBUF 1+ C@ . BLKBUF 2 + C@ . BLKBUF 3 + C@ . CR

BYE
