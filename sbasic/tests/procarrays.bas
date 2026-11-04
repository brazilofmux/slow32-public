REM a SUB's arrays are its own unless SHARED or DIM SHARED; a local
REM one is new each call (checked against QBasic 1.1)
DECLARE SUB Local1 ()
DECLARE SUB Share1 ()
DECLARE SUB Fresh ()
DECLARE SUB Deep (N%)
DECLARE FUNCTION Total% ()
DIM SHARED D(3), Z%
N = 2: DIM A%(5), B(N)
A%(1) = 11: B(1) = 1.5: D(1) = 4: Z% = 7
CALL Local1: PRINT A%(1); UBOUND(A%)
CALL Share1: PRINT B(1); UBOUND(B)
CALL Fresh: CALL Fresh
CALL Deep(3)
PRINT Total%; D(2)
END

SUB Local1
    REDIM A%(3)
    A%(1) = 99: PRINT A%(1);
    REDIM A%(2): PRINT UBOUND(A%);
    DIM C(4): C(4) = 3: PRINT C(4)
END SUB

SUB Share1
    SHARED B()
    B(1) = B(1) * 2: PRINT B(1);
    REDIM B(8)
    B(1) = 5
END SUB

SUB Fresh
    DIM L(3)
    L(1) = L(1) + 1: PRINT L(1)
END SUB

SUB Deep (N%)
    DIM W(1)
    W(1) = N%
    IF N% > 0 THEN CALL Deep(N% - 1)
    PRINT W(1);
    IF N% = 3 THEN PRINT
END SUB

FUNCTION Total%
    D(2) = D(1) * 10
    Total% = Z% + D(1)
END FUNCTION
