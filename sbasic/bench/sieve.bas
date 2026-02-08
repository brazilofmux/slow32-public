' Sieve of Eratosthenes - find primes up to 1000
DIM flags%(1000)
count% = 0
FOR i% = 2 TO 1000
    flags%(i%) = 1
NEXT
FOR i% = 2 TO 1000
    IF flags%(i%) = 1 THEN
        count% = count% + 1
        FOR j% = i% + i% TO 1000 STEP i%
            flags%(j%) = 0
        NEXT
    END IF
NEXT
PRINT "Primes found:"; count%
END
