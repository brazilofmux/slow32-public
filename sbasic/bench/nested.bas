' Nested loop arithmetic (LONG: the sum and i * j pass an INTEGER's 32767)
sum& = 0
FOR i& = 1 TO 500
    FOR j& = 1 TO 500
        sum& = sum& + (i& * j&) MOD 97
    NEXT
NEXT
PRINT "Sum ="; sum&
END
