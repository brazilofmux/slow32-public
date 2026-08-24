\ bench.fth — forth/bench.fth's kernels, verbatim, under a MAIN.
\ Expected: 317811 / 1899 / 1000000.

: FIB  DUP 2 < IF EXIT THEN DUP 1- RECURSE SWAP 2 - RECURSE + ;

CREATE FLAGS 8192 ALLOT

: DO-PRIME
  FLAGS 8192 1 FILL
  0
  8192 0 DO
    FLAGS I + C@
    IF
      I DUP + 3 + DUP I +
      BEGIN DUP 8192 < WHILE
        0 OVER FLAGS + C!
        OVER +
      REPEAT
      DROP DROP
      1+
    THEN
  LOOP
;

: SIEVE  10 0 DO DO-PRIME DROP LOOP DO-PRIME ;

: NESTED
  0
  100 0 DO
    100 0 DO
      100 0 DO
        1+
      LOOP
    LOOP
  LOOP
;

: MAIN
  28 FIB . CR
  SIEVE . CR
  NESTED . CR
;
