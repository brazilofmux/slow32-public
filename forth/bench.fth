\ Forth Performance Benchmark
\ Portable across gforth, pforth, and SLOW-32 Forth kernel

\ --- Recursive Fibonacci ---
: FIB  DUP 2 < IF EXIT THEN DUP 1- RECURSE SWAP 2 - RECURSE + ;

\ --- Sieve of Eratosthenes ---
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

\ --- Nested loop (integer-heavy) ---
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

\ --- Run benchmarks ---
: BENCH
  28 FIB . CR
  SIEVE . CR
  NESTED . CR
;

BENCH BYE
