' Nested loop arithmetic
sum% = 0
FOR i% = 1 TO 500
    FOR j% = 1 TO 500
        sum% = sum% + (i% * j%) MOD 97
    NEXT
NEXT
PRINT "Sum ="; sum%
END
