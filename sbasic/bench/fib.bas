' Recursive Fibonacci - small enough to not overflow stack
FUNCTION Fib%(n%)
    IF n% <= 1 THEN
        Fib% = n%
    ELSE
        Fib% = Fib%(n% - 1) + Fib%(n% - 2)
    END IF
END FUNCTION
PRINT "fib(20) ="; Fib%(20)
END
