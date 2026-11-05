' two arrays of one procedure are two arrays: of each type, DIMmed apart
' or together, in branches, STATIC beside local, element by element, and in
' the loops of a procedure called more than once
DECLARE SUB pair ()
DECLARE SUB fade (d%, p&())
DECLARE SUB mixed ()
DECLARE SUB stat (v%)
DEFINT A-Z
DIM q&(0 TO 3)
FOR i = 0 TO 3: q&(i) = 4128768 + i * 256 + 63: NEXT
pair
fade 0, q&()
fade 1, q&()
fade 0, q&()
mixed
stat 1
stat 2
PRINT "done"
SYSTEM

SUB pair
    DIM alpha(3)
    DIM beta(3), gamma(1 TO 3)
    alpha(1) = 5: beta(1) = 1: gamma(1) = 9
    beta(2) = 7
    PRINT alpha(1); beta(1); gamma(1); alpha(2); beta(2); gamma(2)
    PRINT UBOUND(alpha); UBOUND(beta); LBOUND(gamma)
END SUB

SUB fade (d, p&())
IF d = 0 THEN
    DIM r!(0 TO 3, 0 TO 2)
    DIM s!(0 TO 3, 0 TO 2)
    FOR n = 0 TO 3
        c& = p&(n)
        b = c& \ 65536: c& = c& - b * 65536
        g = c& \ 256: c& = c& - g * 256
        v = c&
        r!(n, 0) = v: r!(n, 1) = g: r!(n, 2) = b
        s!(n, 0) = v / 63: s!(n, 1) = g / 63: s!(n, 2) = b / 63
    NEXT n
    FOR j = 1 TO 63
        FOR n = 0 TO 3
            r!(n, 0) = r!(n, 0) - s!(n, 0)
            r!(n, 1) = r!(n, 1) - s!(n, 1)
            r!(n, 2) = r!(n, 2) - s!(n, 2)
        NEXT n
    NEXT j
    PRINT r!(1, 0); r!(1, 1); r!(3, 2)
ELSE
    DIM r!(0 TO 3, 0 TO 2)
    DIM a!(0 TO 3, 0 TO 2)
    FOR n = 0 TO 3
        c& = p&(n)
        b = c& \ 65536: c& = c& - b * 65536
        g = c& \ 256: c& = c& - g * 256
        v = c&
        a!(n, 0) = v / 63: a!(n, 1) = g / 63: a!(n, 2) = b / 63
    NEXT n
    FOR j = 1 TO 63
        FOR n = 0 TO 3
            r!(n, 0) = r!(n, 0) + a!(n, 0)
            r!(n, 1) = r!(n, 1) + a!(n, 1)
            r!(n, 2) = r!(n, 2) + a!(n, 2)
        NEXT n
    NEXT j
    PRINT r!(1, 0); r!(1, 1); r!(3, 2)
END IF
END SUB

SUB mixed
    DIM n%(2), d#(2), w$(2)
    n%(1) = 3: d#(1) = 1.5: w$(1) = "w"
    FOR i = 1 TO 2
        n%(2) = n%(2) + n%(1): d#(2) = d#(2) + d#(1): w$(2) = w$(2) + w$(1)
    NEXT i
    PRINT n%(2); d#(2); w$(2)
END SUB

SUB stat (v)
    STATIC keepa(), keepb()
    DIM own(2)
    STATIC done
    IF done = 0 THEN DIM keepa(2): DIM keepb(2): done = 1
    keepa(1) = keepa(1) + v: keepb(1) = keepb(1) + 10 * v: own(1) = own(1) + 100
    PRINT keepa(1); keepb(1); own(1)
END SUB
