' SUB and FUNCTION in s32-bc: by reference, by value, elements copied
' back, results of each type, STATIC, SHARED, recursion, EXIT
DECLARE SUB Bump (N%, S$, D#, X)
DIM A%(5), W$(3), Q(4)
I% = 1: T$ = "t": D# = 1 / 3: X = 2.5
CALL Bump(I%, T$, D#, X): PRINT I%; T$; D#; X
CALL Bump((I%), (T$), (D#), (X)): PRINT I%; T$; D#; X
A%(2) = 7: W$(1) = "w": Q(3) = 1.5
CALL Bump(A%(2), W$(1), D#, Q(3)): PRINT A%(2); W$(1); Q(3)
CALL Bump(I% + 1, T$ + "", 2, 3): PRINT I%; T$
CALL Shrink(A%(5)): PRINT "after shrink"; A%(5); UBOUND(A%)
PRINT Fib&(20); Twice$("ab"); Third#(1); Half(3); Never%(1); Nothing$(2); "|"
PRINT Fib&(Fib&(5) + 1)
FOR K = 1 TO 3: PRINT Counter%;: NEXT: PRINT
FOR K = 1 TO 3: CALL Logit("x" + STR$(K)): NEXT: CALL Logit("")
Total = 5: CALL AddShared(10): CALL AddShared(1): PRINT Total; Unseen
PRINT Early%(3); Early%(-3)
PRINT Kinds$(2); Kinds$(7)
CALL Reader: CALL Reader
J% = 3: PRINT Sq%(J%); J%; Sq%((J%)); J%
PRINT Nested(Nested(2))
DATA 11, 22
END

SUB Bump (N%, S$, D#, X)
    N% = N% + 1: S$ = S$ + "!": D# = D# * 3: X = X / 2
END SUB

SUB Shrink (E%)
    E% = 99
END SUB

FUNCTION Fib& (N&)
    IF N& < 2 THEN Fib& = N& ELSE Fib& = Fib&(N& - 1) + Fib&(N& - 2)
END FUNCTION

FUNCTION Twice$ (S$)
    Twice$ = S$ + S$
END FUNCTION

FUNCTION Third# (V#)
    Third# = V# / 3
END FUNCTION

FUNCTION Half (V)
    Half = V / 2
END FUNCTION

FUNCTION Never% (V)
END FUNCTION

FUNCTION Nothing$ (V)
END FUNCTION

FUNCTION Counter%
    STATIC C%
    C% = C% + 1: Counter% = C% * 10
END FUNCTION

SUB Logit (M$)
    STATIC L$, N
    IF M$ = "" THEN PRINT L$; N: EXIT SUB
    L$ = L$ + M$: N = N + 1
END SUB

SUB AddShared (V)
    SHARED Total, Unseen
    Total = Total + V: Unseen = Unseen + 1
END SUB

FUNCTION Early% (V)
    Early% = -1
    IF V < 0 THEN EXIT FUNCTION
    Early% = V * 2
END FUNCTION

FUNCTION Kinds$ (V)
    SELECT CASE V
    CASE IS < 5: Kinds$ = "small"
    CASE ELSE: Kinds$ = "big"
    END SELECT
END FUNCTION

SUB Reader
    READ R: PRINT R
END SUB

FUNCTION Sq% (N%)
    Sq% = N% * N%: N% = N% + 1
END FUNCTION

FUNCTION Nested (V)
    LOCALV = V * 10
    Nested = LOCALV + 1
END FUNCTION
